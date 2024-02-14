using System;
using System.Collections.Generic;
using System.Linq;

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

            var parser = new parser(line);
            var _syntree = syntree.parse(line);

            if(showtree)
                pp(_syntree.root);

            if (_syntree.diags.Any())
            {
                Console.ForegroundColor = ConsoleColor.Red;

                foreach (var diag in _syntree.diags)
                    Console.WriteLine(diag);

                Console.ResetColor();
            }
            else {
                var e = new evaler(_syntree.root);
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
    unary
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
    public numsyn(syntoken numtok) {
        this.numtok = numtok;
    }

    public override syntype type => syntype.numexpr;
    public syntoken numtok { get; }

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
        if (cur.type == syntype.lpar) {
            var l = nextok();
            var expr = parseexpr();
            var r = match(syntype.rpar);
            return new parensyn(l, expr, r);
        }

        var numtok = match(syntype.num);
        return new numsyn(numtok);
    }
}

class evaler {
    readonly exprsyn _root;

    public evaler(exprsyn root) { 
        this._root = root;
    }

    public int eval() {
        return evalexpr(_root);
    }

    int evalexpr(exprsyn root) {
        //bin expr
        //num expr

        if (root is numsyn n)
            return (int)n.numtok.val;

        if (root is unaryexprsyn u) {
            var operand = evalexpr(u.operand);

            switch (u.oper.type) {
                case syntype.plus:
                    return operand;
                case syntype.minus:
                    return -operand;
            }

            throw new Exception($"unexpected unary oper {u.oper.type}");
        }

        if (root is binexprsyn b) {
            var l = evalexpr(b.l);
            var r = evalexpr(b.r);

            switch (b.oper.type) {
                case syntype.plus:
                    return l + r;
                case syntype.minus:
                    return l - r;
                case syntype.mult:
                    return l * r;
                case syntype.div:
                    return l / r;
            }

            throw new Exception($"unexpected bin oper {b.oper.type}");
        }

        if (root is parensyn p)
            return evalexpr(p.expr);

        throw new Exception($"unexpected node {root.type}");
    }
}