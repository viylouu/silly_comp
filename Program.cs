using System;
using System.Collections.Generic;
using System.Linq;

class main {
    static void Main() {
        bool showtree = false;

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

                Console.ForegroundColor = ConsoleColor.White;
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
        Console.ForegroundColor = ConsoleColor.White;

        Console.Write(node.type);

        if (node is syntoken t && t.val != null) {
            Console.Write(" ");
            Console.Write(t.val);
        }

        Console.WriteLine();

        ind += last ? "    " : "|   ";

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
    parenexpr
}

class syntoken : synnode {
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

class lexer {
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

class parser {
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
        var expr = parseterm();
        var eof = match(syntype.eof);
        return new syntree(_diags, expr, eof);
    }

    public exprsyn parseterm() {
        var l = parsefac();

        while (cur.type == syntype.plus || cur.type == syntype.minus) {
            var oper = nextok();
            var r = parsefac();
            l = new binexprsyn(l, oper, r);
        }

        return l;
    }

    public exprsyn parsefac() {
        var l = parsepriexpr();

        while (cur.type == syntype.mult || cur.type == syntype.div) {
            var oper = nextok();
            var r = parsepriexpr();
            l = new binexprsyn(l, oper, r);
        }

        return l;
    }

    exprsyn parsepriexpr() {
        if (cur.type == syntype.lpar) {
            var l = nextok();
            var expr = parseterm();
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