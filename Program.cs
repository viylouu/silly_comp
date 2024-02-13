class main {
    static void Main() {
        while (true) {
            Console.Write("> ");

            var line = Console.ReadLine();
            if (string.IsNullOrWhiteSpace(line))
                return;

            var lexer = new lexer(line);
            while (true) {
                var tok = lexer.nextok();
                if (tok.type == syntype.eof)
                    break;
                Console.Write($"{tok.type}: '{tok.text}'");
                if (tok.val != null)
                    Console.Write($" {tok.val}");

                Console.WriteLine();
            }
        }
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
    numexpr
}

class syntoken {
    public syntype type { get; }
    public int pos { get; }
    public string text { get; }
    public object val { get; }

    public syntoken(syntype type, int pos, string text, object val) { 
        this.type = type; this.text = text; this.pos = pos; this.val = val;
    }
}

class lexer {
    readonly string _text;
    int _pos;

    public lexer(string text) { _text = text; }

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
            int.TryParse(tex, out var val);
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

        return new syntoken(syntype.uhoh, _pos++, _text.Substring(_pos - 1, 1), null);
    }
}

abstract class synnode { 
    public abstract syntype type { get; }
}

abstract class exprsyn : synnode { 
    
}

sealed class numsyn : exprsyn {
    public numsyn(syntoken numtok) { 
    }

    public override syntype type => syntype.numexpr;
}

class parser {
    readonly syntoken[] _toks;
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
    }

    syntoken peek(int off) {
        var idx = _pos + off;

        if (idx >= _toks.Length)
            return _toks[_toks.Length - 1];

        return _toks[idx];
    }

    syntoken cur => peek(0);
}