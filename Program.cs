using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection.Emit;

internal class main {
    static void Main() {
        var showtree = false;

        while (true) {
            Console.Write("> ");

            var line = Console.ReadLine();
            if (string.IsNullOrWhiteSpace(line))
                return;

            if (line == "#showtree") {
                showtree = !showtree;

                Console.WriteLine(showtree?"showing trees" : "not showing trees");

                continue;
            }
            if (line == "#clr") { Console.Clear(); continue; }

            var _syntree = syntree.parse(line);
            var _binder = new binder();
            var boundexpr = _binder.bindexpr(_syntree.root);

            var diags = _syntree.diags.Concat(_binder.diags).ToArray();

            if (showtree)
                pp(_syntree.root);

            if (diags.Any()) {
                Console.ForegroundColor = ConsoleColor.Red;

                foreach (var diag in diags)
                    Console.WriteLine(diag);

                Console.ResetColor();
            }
            else {
                var e = new evaler(boundexpr);
                var res = e.eval();
                Console.WriteLine(res);
            }
        }
    }

    static void pp(synnode node, string ind = "", bool last = true) {
        //└──
        //├──
        //|

        var mark = last ? "└──" : "├──";

        Console.ForegroundColor = ConsoleColor.DarkGray;
        Console.Write(ind);
        Console.Write(mark);
        Console.ResetColor();

        Console.Write(node.type);

        if (node is syntoken t && t.val != null) {
            Console.Write(" ");
            Console.Write(t.val);
        }

        Console.WriteLine();

        ind += last ? "   " : "|  ";

        var lastkid = node.getchildren().LastOrDefault();

        foreach (var chi in node.getchildren())
            pp(chi, ind, chi == lastkid);
    }
}

enum syntype {
    num,
    ws,
    plus,
    minus,
    mult,
    div,
    lpar,
    rpar,
    uhoh,
    eof,
    numexpr,
    bin,
    parenexpr,
    unary,
    truekw,
    falsekw,
    identif
}

sealed class syntoken : synnode {
    public override syntype type { get; }
    public int pos { get; }
    public string text { get; }
    public object val { get; }

    public syntoken(syntype type, int pos, string text, object val) {
        this.type = type; this.text = text; this.pos = pos; this.val = val;
    }

    public override IEnumerable<synnode> getchildren() { 
        return Enumerable.Empty<synnode>();
    }
}

internal sealed class lexer {
    readonly string _text;
    int _pos;
    List<string> _diags = new List<string>();

    public lexer(string text) { _text = text; }

    public IEnumerable<string> diags => _diags;

    char cur {
        get {
            if (_pos >= _text.Length)
                return '\0';

            return _text[_pos];
        }
    }

    void nex() { _pos++; }

    public syntoken nextok() {
        // nums
        // +-*/
        // ws

        if (_pos >= _text.Length)
            return new syntoken(syntype.eof, _pos, "\0", null);

        if (char.IsDigit(cur)) {
            var star = _pos;
            while (char.IsDigit(cur))
                nex();
            var len = _pos - star;
            var tex = _text.Substring(star, len);
            if (!int.TryParse(tex, out var val))
                _diags.Add($"{_text} isnt a valid int32");
            return new syntoken(syntype.num, star, tex, val);
        }

        if (char.IsWhiteSpace(cur)) {
            var star = _pos;
            while (char.IsWhiteSpace(cur))
                nex();
            var len = _pos - star;
            var tex = _text.Substring(star, len);
            return new syntoken(syntype.ws, star, tex, null);
        }

        if (char.IsLetter(cur)) {
            var star = _pos;
            while (char.IsLetter(cur))
                nex();
            var len = _pos - star;
            var tex = _text.Substring(star, len);
            var type = synfacts.getkeywordtype(tex);
            return new syntoken(type, star, tex, null);
        }

        switch (cur) {
            case '+':
                return new syntoken(syntype.plus, _pos++, "+", null);
            case '-':
                return new syntoken(syntype.minus, _pos++, "-", null);
            case '*':
                return new syntoken(syntype.mult, _pos++, "*", null);
            case '/':
                return new syntoken(syntype.div, _pos++, "/", null);
            case '(':
                return new syntoken(syntype.lpar, _pos++, "(", null);
            case ')':
                return new syntoken(syntype.rpar, _pos++, ")", null);
        }

        _diags.Add($"err: bad char in: '{cur}'");
        return new syntoken(syntype.uhoh, _pos++, _text.Substring(_pos - 1, 1), null);
    }
}

abstract class synnode { 
    public abstract syntype type { get; }

    public abstract IEnumerable<synnode> getchildren();
}

abstract class exprsyn : synnode { 
    
}

sealed class numsyn : exprsyn {
    public numsyn(syntoken numtok) : this(numtok, numtok.val) { }

    public numsyn(syntoken numtok, object val) {
        this.numtok = numtok; this.val = val;
    }

    public override syntype type => syntype.numexpr;
    public syntoken numtok { get; }
    public object val { get; }

    public override IEnumerable<synnode> getchildren() {
        yield return numtok;
    }
}

sealed class binexprsyn : exprsyn {
    public binexprsyn(exprsyn l, syntoken oper, exprsyn r) {
        this.l = l; this.r = r; this.oper = oper;
    }

    public exprsyn l { get; }
    public syntoken oper { get; }
    public exprsyn r { get; }

    public override syntype type => syntype.bin;

    public override IEnumerable<synnode> getchildren() {
        yield return l;
        yield return oper;
        yield return r;
    }
}

sealed class unaryexprsyn : exprsyn {
    public unaryexprsyn(syntoken oper, exprsyn operand) {
        this.oper = oper; this.operand = operand;
    }

    public syntoken oper { get; }
    public exprsyn operand { get; }

    public override syntype type => syntype.unary;

    public override IEnumerable<synnode> getchildren() {
        yield return oper;
        yield return operand;
    }
}

sealed class parensyn : exprsyn {
    public parensyn(syntoken l, exprsyn expr, syntoken r) {
        this.l = l; this.expr = expr; this.r = r;
    }

    public syntoken l { get; }
    public exprsyn expr { get; }
    public syntoken r { get; }

    public override syntype type => syntype.parenexpr;

    public override IEnumerable<synnode> getchildren() {
        yield return l;
        yield return expr;
        yield return r;
    }
}

sealed class syntree {
    public syntree(IEnumerable<string> diags, exprsyn root, syntoken eof) {
        this.diags = diags.ToArray(); this.root = root; this.eof = eof;
    }

    public IReadOnlyList<string> diags { get; }
    public exprsyn root { get; }
    public syntoken eof { get; }

    public static syntree parse(string text) {
        var parser = new parser(text);
        return parser.parse();
    }
}

internal static class synfacts { 
    public static int getbinoperprec(this syntype type) {
        switch (type) {
            case syntype.mult:
            case syntype.div:
                return 2;

            case syntype.plus:
            case syntype.minus:
                return 1;

            default:
                return 0;
        }
    }

    public static int getunaryoperprec(this syntype type) {
        switch (type) {
            case syntype.plus:
            case syntype.minus:
                return 3;

            default:
                return 0;
        }
    }

    public static syntype getkeywordtype(string tex) {
        switch (tex) {
            case "true":
                return syntype.truekw;
            case "false":
                return syntype.falsekw;
            default:
                return syntype.identif;
        }
    }
}

internal sealed class parser {
    readonly syntoken[] _toks;
    List<string> _diags = new List<string>();
    int _pos;

    public parser(string text) { 
        var toks = new List<syntoken>();

        var lex = new lexer(text);
        syntoken tok;

        do {
            tok = lex.nextok();

            if (tok.type != syntype.ws && tok.type != syntype.uhoh) {
                toks.Add(tok);
            }
        } while (tok.type != syntype.eof);

        _toks = toks.ToArray();
        _diags.AddRange(lex.diags);
    }

    syntoken peek(int off) {
        var idx = _pos + off;

        if (idx >= _toks.Length)
            return _toks[_toks.Length - 1];

        return _toks[idx];
    }

    public IEnumerable<string> diags => _diags;

    syntoken cur => peek(0);

    syntoken nextok() {
        var _cur = cur;
        _pos++;
        return _cur;
    }

    syntoken match(syntype type) {
        if (cur.type == type)
            return nextok();

        _diags.Add($"err: unexpected tok <{cur.type}>, expected <{type}>");
        return new syntoken(type, cur.pos, null, null);
    }

    public syntree parse() {
        var expr = parseexpr();
        var eof = match(syntype.eof);
        return new syntree(_diags, expr, eof);
    }

    exprsyn parseexpr(int parprec = 0) {
        exprsyn l;
        var unaryoperprec = cur.type.getunaryoperprec();

        if (unaryoperprec != 0 && unaryoperprec >= parprec) {
            var opertok = nextok();
            var operand = parseexpr(unaryoperprec);
            l = new unaryexprsyn(opertok, operand);
        } else
            l = parsepriexpr();

        while (true) {
            var prec = cur.type.getbinoperprec();

            if (prec == 0 || prec <= parprec) break;

            var opertok = nextok();
            var r = parseexpr(prec);
            l = new binexprsyn(l, opertok, r);
        }

        return l;
    }

    exprsyn parsepriexpr() {
        switch (cur.type) {
            case syntype.lpar: {
                var l = nextok();
                var expr = parseexpr();
                var r = match(syntype.rpar);
                return new parensyn(l, expr, r);
            }

            case syntype.truekw:
            case syntype.falsekw: {
                var kwtok = nextok();
                var val = cur.type == syntype.truekw;
                return new numsyn(kwtok, val);
            }

            default: {
                var numtok = match(syntype.num);
                return new numsyn(cur, numtok.val);
            }
        }
    }
}

internal sealed class evaler {
    readonly boundexpr _root;

    public evaler(boundexpr root) { 
        _root = root;
    }

    public object eval() {
        return evalexpr(_root);
    }

    object evalexpr(boundexpr root) {
        //bin expr
        //num expr

        if (root is boundnumexpr n)
            return n.val;

        if (root is boundunaryexpr u) {
            var operand = (int)evalexpr(u.operand);

            switch (u.opertype) {
                case boundunaryopertype.ident:
                    return operand;
                case boundunaryopertype.negate:
                    return -operand;
            }

            throw new Exception($"unexpected unary oper {u.opertype}");
        }

        if (root is boundbinexpr b) {
            var l = (int)evalexpr(b.l);
            var r = (int)evalexpr(b.r);

            switch (b.opertype) {
                case boundbinopertype.add:
                    return l + r;
                case boundbinopertype.sub:
                    return l - r;
                case boundbinopertype.mul:
                    return l * r;
                case boundbinopertype.div:
                    return l / r;
            }

            throw new Exception($"unexpected bin oper {b.opertype}");
        }

        throw new Exception($"unexpected node {root.type}");
    }
}

internal abstract class boundnode { 
    public abstract boundnodetype type { get; }

}

internal enum boundnodetype {
    unary,
    num
}

internal abstract class boundexpr : boundnode { 
    public abstract Type type_ { get; }
}

internal enum boundunaryopertype { 
    ident,
    negate
}

internal sealed class boundnumexpr : boundexpr {
    public boundnumexpr(object val) {
        this.val = val;
    }

    public object val { get; }

    public override Type type_ => val.GetType();
    public override boundnodetype type => boundnodetype.num;
}

internal sealed class boundunaryexpr : boundexpr {
    public boundunaryexpr(boundunaryopertype opertype, boundexpr operand) { 
        this.opertype = opertype; this.operand = operand;
    }

    public boundunaryopertype opertype { get; }
    public boundexpr operand { get; }

    public override Type type_ => operand.type_;
    public override boundnodetype type => boundnodetype.unary;
}

internal enum boundbinopertype { 
    add,
    sub,
    mul,
    div,
}

internal sealed class boundbinexpr : boundexpr {
    public boundbinexpr(boundexpr l, boundbinopertype opertype, boundexpr r) {
        this.l = l; this.opertype = opertype; this.r = r;
    }

    public boundexpr l { get; }
    public boundbinopertype opertype { get; }
    public boundexpr r { get; }

    public override Type type_ => l.type_;
    public override boundnodetype type => boundnodetype.unary;
}

internal sealed class binder {
    readonly List<string> _diags = new List<string>();
    
    public IEnumerable<string> diags => _diags;

    public boundexpr bindexpr(exprsyn syn) {
        switch (syn.type) {
            case syntype.bin:
                return bindbinexpr((binexprsyn)syn);
            case syntype.unary:
                return bindunaryexpr((unaryexprsyn)syn);
            case syntype.numexpr:
                return bindnumexpr((numsyn)syn);
            default:
                throw new Exception($"unexpected syntax {syn.type}");
        }
    }

    boundexpr bindnumexpr(numsyn syn) {
        var val = syn.val ?? 0;
        return new boundnumexpr(val);
    }

    boundexpr bindunaryexpr(unaryexprsyn syn) {
        var boundoperand = bindexpr(syn.operand);
        var boundopertype = bindunaryopertype(syn.oper.type, boundoperand.type_);
        if (boundopertype == null) {
            _diags.Add($"unary oper '{syn.oper.text}' is not defined for type {boundoperand.type_}");
            return boundoperand;
        }
        return new boundunaryexpr(boundopertype.Value, boundoperand);
    }

    boundunaryopertype? bindunaryopertype(syntype type, Type operandtype) {
        if (operandtype != typeof(int))
            return null;

        switch (type) {
            case syntype.plus:
                return boundunaryopertype.ident;
            case syntype.minus:
                return boundunaryopertype.negate;
            default:
                throw new Exception($"unexpected unary oper {type}");
        }
    }

    boundexpr bindbinexpr(binexprsyn syn) {
        var boundl = bindexpr(syn.l);
        var boundr = bindexpr(syn.r);
        var boundopertype = bindbinopertype(syn.oper.type, boundl.type_, boundr.type_);
        if (boundopertype == null) {
            _diags.Add($"bin oper '{syn.oper.text}' is not defined for types {boundl.type_} and {boundr.type_}");
            return boundl;
        }
        return new boundbinexpr(boundl, boundopertype.Value, boundr);
    }

    boundbinopertype? bindbinopertype(syntype type, Type ltype, Type rtype) {
        if (ltype != typeof(int) || rtype != typeof(int))
            return null;

        switch (type) {
            case syntype.plus:
                return boundbinopertype.add;
            case syntype.minus:
                return boundbinopertype.sub;
            case syntype.mult:
                return boundbinopertype.mul;
            case syntype.div:
                return boundbinopertype.div;
            default:
                throw new Exception($"unexpected bin oper {type}");
        }
    }
}