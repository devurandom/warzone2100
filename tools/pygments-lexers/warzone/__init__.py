from pygments.lexer import RegexLexer
from pygments.token import *

class WRFLexer(RegexLexer):
    name = 'WRF'
    aliases = ['wrf']
    filenames = ['*.wrf']
    mimetypes = ['text/x-wrf']

    tokens = {
        'root': [
            (r'/\*', Comment.Multiline, 'comment'),
            (r'//.*?$', Comment.Singleline),
            (r'\bdirectory\b', Keyword),
            (r'\bfile\b', Keyword, 'file_line'),
            (r'"[^"]*"', String),
            (r'[ \t\n\x0d\x0a]+', Text),
        ],
        'comment': [
            (r'[^*]+', Comment.Multiline),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'\*[^/]', Comment.Multiline),
        ],
        'file_line': [
            (r'[ \t\n\x0d\x0a]+', Text),
            (r'\b.+?\b', Literal, '#pop'),
        ]
    }

class STRRESLexer(RegexLexer):
    name = 'STRRES'
    aliases = ['strres']
    filenames = ['*.txt']
    mimetypes = ['text/x-strres']

    tokens = {
        'root': [
            (r'/\*', Comment.Multiline, 'comment'),
            (r'//.*?$', Comment.Singleline),
            (r'"[^"]*"', String),
            (r'[_a-zA-Z][-0-9_a-zA-Z]*', Literal),
            (r'[ \t\n\x0d\x0a]+', Text),
            (r'\(', Punctuation, '#push'),
            (r'\)', Punctuation, '#pop'),
        ],
        'comment': [
            (r'[^*]+', Comment.Multiline),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'\*[^/]', Comment.Multiline),
        ],
    }
