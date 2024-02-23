using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Reflection.Emit;
using static System.Net.Mime.MediaTypeNames;

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
            if (line == "#loadtest") {
                using (StreamReader sr = new StreamReader("../../../test.silly"))
                    line = sr.ReadToEnd();
            }

            var _syntree = syntree.parse(line);
            var _comp = new comp(_syntree);
            var res = _comp.eval();

            var diags = res.diags;

            if (showtree)
                pp(_syntree.root);

            if (diags.Any()) {
                Console.ForegroundColor = ConsoleColor.Red;

                foreach (var diag in diags)
                    Console.WriteLine(diag);

                Console.ResetColor();
            }
            else
                Console.WriteLine(res.val);
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

public enum syntype {
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
    identif,
    not,
    and,
    or,
    eqto,
    neqto
}

public sealed class syntoken : synnode {
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
    diagbag _diags = new diagbag();

    public lexer(string text) { _text = text; }

    public diagbag diags => _diags;

    char cur => peek(0);
    char ahead => peek(1);

    char peek(int off) {
        var idx = _pos + off;
        if (idx >= _text.Length)
            return '\0';

        return _text[_pos];
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
                _diags.repinvnum(new textspan(star, len), _text, typeof(int));
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
            //math
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

            //bools
            case '!':
                if (ahead == '=')
                    return new syntoken(syntype.neqto, _pos += 2, "!=", null);
                return new syntoken(syntype.not, _pos++, "!", null);
            case '&':
                if (ahead == '&')
                    return new syntoken(syntype.and, _pos+=2, "&&", null);
                break;
            case '|':
                if (ahead == '|')
                    return new syntoken(syntype.or, _pos += 2, "||", null);
                break;
            case '=':
                if (ahead == '=')
                    return new syntoken(syntype.eqto, _pos += 2, "==", null);
                break;

        }

        _diags.repbadchar(_pos, cur);
        return new syntoken(syntype.uhoh, _pos++, _text.Substring(_pos - 1, 1), null);
    }
}

public abstract class synnode { 
    public abstract syntype type { get; }

    public abstract IEnumerable<synnode> getchildren();
}

public abstract class exprsyn : synnode { 
    
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

public sealed class syntree {
    public syntree(IEnumerable<diag> diags, exprsyn root, syntoken eof) {
        this.diags = diags.ToArray(); this.root = root; this.eof = eof;
    }

    public IReadOnlyList<diag> diags { get; }
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
                return 5;

            case syntype.plus:
            case syntype.minus:
                return 4;

            case syntype.eqto:
            case syntype.neqto:
                return 3;

            case syntype.and:
                return 2;
            case syntype.or:
                return 1;

            default:
                return 0;
        }
    }

    public static int getunaryoperprec(this syntype type) {
        switch (type) {
            case syntype.minus:
            case syntype.not:
                return 6;

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
    diagbag _diags = new diagbag();
    int _pos;

    public parser(string text) { 
        var toks = new List<syntoken>();

        var lex = new lexer(text);
        syntoken tok;

        do {
            tok = lex.nextok();

            if (tok.type != syntype.ws && tok.type != syntype.uhoh)
                toks.Add(tok);
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

    public IEnumerable<diag> diags => _diags;

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
                var val = kwtok.type == syntype.truekw;
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
        if (root is boundnumexpr n)
            return n.val;

        if (root is boundunaryexpr u) {
            var operand = evalexpr(u.operand);

            switch (u.opertype.type) {
                case boundunaryopertype.ident:
                    return (int)operand;
                case boundunaryopertype.negate:
                    return -(int)operand;
                case boundunaryopertype.lognegate:
                    return !(bool)operand;
            }

            throw new Exception($"unexpected unary oper {u.opertype}");
        }

        if (root is boundbinexpr b) {
            var l = evalexpr(b.l);
            var r = evalexpr(b.r);

            switch (b.opertype.mtype) {
                case boundbinopertype.add:
                    return (int)l + (int)r;
                case boundbinopertype.sub:
                    return (int)l - (int)r;
                case boundbinopertype.mul:
                    return (int)l * (int)r;
                case boundbinopertype.div:
                    return (int)l / (int)r;
                case boundbinopertype.logand:
                    return (bool)l && (bool)r;
                case boundbinopertype.logor:
                    return (bool)l || (bool)r;
                case boundbinopertype.eqto:
                    return Equals(l, r);
                case boundbinopertype.neqto:
                    return !Equals(l, r);
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
    num,
    bin
}

internal abstract class boundexpr : boundnode { 
    public abstract Type type_ { get; }
}

internal enum boundunaryopertype { 
    ident,
    negate,
    lognegate
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
    public boundunaryexpr(boundunaryoper opertype, boundexpr operand) { 
        this.opertype = opertype; this.operand = operand;
    }

    public boundunaryoper opertype { get; }
    public boundexpr operand { get; }

    public override Type type_ => opertype.restype;
    public override boundnodetype type => boundnodetype.unary;
}

internal enum boundbinopertype { 
    add,
    sub,
    mul,
    div,
    logand,
    logor,
    eqto,
    neqto,
}

internal sealed class boundbinexpr : boundexpr {
    public boundbinexpr(boundexpr l, boundbinoper opertype, boundexpr r) {
        this.l = l; this.opertype = opertype; this.r = r;
    }

    public boundexpr l { get; }
    public boundbinoper opertype { get; }
    public boundexpr r { get; }

    public override Type type_ => opertype.mrestype;
    public override boundnodetype type => boundnodetype.bin;
}

public class comp {
    public comp(syntree syn) { 
        this.syn = syn;
    }

    public syntree syn { get; }

    public evalres eval() {
        var binder = new binder();
        var boundexpr = binder.bindexpr(syn.root);

        var diags = syn.diags.Concat(binder.diags).ToArray();
        if (diags.Any())
            return new evalres(diags, null);

        var evaler = new evaler(boundexpr);
        var val = evaler.eval();
        return new evalres(Array.Empty<string>(), val);
    }
}

public struct textspan {
    public textspan(int start, int len) { 
        this.start = start; this.len = len;
    }

    public int start { get; }
    public int len { get; }
    public int end => start + len;
}

public sealed class diag {
    public diag(textspan span, string msg) {
        this.span = span; this.msg = msg;
    }

    public textspan span { get; }
    public string msg { get; }

    public override string ToString() => msg;
}

internal sealed class diagbag : IEnumerable<diag> {
    readonly List<diag> _diags = new List<diag>();

    public IEnumerator<diag> GetEnumerator() => _diags.GetEnumerator();

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

    void rep(textspan span, string msg) {
        var diag = new diag(span, msg);
        _diags.Add(diag);
    }

    public void repinvnum(textspan span, string text, Type type) {
        var msg = $"the num {text} isn't a valid {type}";
        rep(span, msg);
    }

    public void repbadchar(int pos, char chr) {
        var span = new textspan(pos, 1);
        var msg = $"err: bad char inp '{chr}'";
        rep(span, msg);
    }

    public void addrange(diagbag diags) {
        _diags.AddRange(diags._diags);
    }
}

public sealed class evalres {
    public evalres(IEnumerable<string> diags, object val) {
        this.diags = diags.ToArray(); this.val = val;
    }

    public IReadOnlyList<string> diags { get; }
    public object val { get; }
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
            case syntype.parenexpr:
                return bindexpr(((parensyn)syn).expr);
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
        var boundopertype = boundunaryoper.bind(syn.oper.type, boundoperand.type_);
        if (boundopertype == null) {
            _diags.Add($"unary oper '{syn.oper.text}' is not defined for type {boundoperand.type_}");
            return boundoperand;
        }
        return new boundunaryexpr(boundopertype, boundoperand);
    }

    boundexpr bindbinexpr(binexprsyn syn) {
        var boundl = bindexpr(syn.l);
        var boundr = bindexpr(syn.r);
        var boundopertype = boundbinoper.bind(syn.oper.type, boundl.type_, boundr.type_);
        if (boundopertype == null) {
            _diags.Add($"bin oper '{syn.oper.text}' is not defined for types {boundl.type_} and {boundr.type_}");
            return boundl;
        }
        return new boundbinexpr(boundl, boundopertype, boundr);
    }
}

internal sealed class boundbinoper {
    boundbinoper(syntype stype, boundbinopertype btype, Type type) : this(stype, btype, type, type, type) { }

    boundbinoper(syntype stype, boundbinopertype btype, Type operand, Type res) : this(stype, btype, operand, operand, res) { }

    boundbinoper(syntype stype, boundbinopertype type, Type ltype, Type rtype, Type restype) {
        mstype = stype; mtype = type; mltype = ltype; mrtype = rtype; mrestype = restype;
    }

    public syntype mstype { get; }
    public boundbinopertype mtype { get; }
    public Type mltype { get; }
    public Type mrtype { get; }
    public Type mrestype { get; }

    static boundbinoper[] _opers = {
        new boundbinoper(syntype.plus, boundbinopertype.add, typeof(int)),
        new boundbinoper(syntype.minus, boundbinopertype.sub, typeof(int)),
        new boundbinoper(syntype.mult, boundbinopertype.mul, typeof(int)),
        new boundbinoper(syntype.div, boundbinopertype.div, typeof(int)),

        new boundbinoper(syntype.eqto, boundbinopertype.eqto, typeof(int), typeof(bool)),
        new boundbinoper(syntype.neqto, boundbinopertype.neqto, typeof(int), typeof(bool)),

        new boundbinoper(syntype.and, boundbinopertype.logand, typeof(bool)),
        new boundbinoper(syntype.or, boundbinopertype.logor, typeof(bool)),

        new boundbinoper(syntype.eqto, boundbinopertype.eqto, typeof(bool)),
        new boundbinoper(syntype.neqto, boundbinopertype.neqto, typeof(bool)),
    };

    public static boundbinoper bind(syntype type, Type ltype, Type rtype) {
        foreach (var oper in _opers)
            if (oper.mstype == type && oper.mltype == ltype && oper.mrtype == rtype)
                return oper;

        return null;
    }
}

internal sealed class boundunaryoper {
    boundunaryoper(syntype stype, boundunaryopertype type, Type operandtype) : this(stype, type, operandtype, operandtype) { }

    boundunaryoper(syntype stype, boundunaryopertype type, Type operandtype, Type restype) {
        this.stype = stype; this.type = type; this.operandtype = operandtype; this.restype = restype;
    }

    public syntype stype { get; }
    public boundunaryopertype type { get; }
    public Type operandtype { get; }
    public Type restype { get; }

    static boundunaryoper[] _opers = {
        new boundunaryoper(syntype.not, boundunaryopertype.lognegate, typeof(bool)),
        new boundunaryoper(syntype.plus, boundunaryopertype.ident, typeof(int)),
        new boundunaryoper(syntype.minus, boundunaryopertype.negate, typeof(int)),
    };

    public static boundunaryoper bind(syntype type, Type operandtype) {
        foreach (var oper in _opers)
            if (oper.stype == type && oper.operandtype == operandtype)
                return oper;

        return null;
    }
}