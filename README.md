# hmagma

```
[ Source code (.hmag) ]
           │
           ▼
     ┌───────────┐
     │   Lexer   │ ──► Tokens (Tokens.hs)
     └───────────┘
           │
           ▼
     ┌───────────┐
     │  Parser   │ ──► AST (AST.hs)
     └───────────┘
           │
           ▼
┌──────────────────┐
│  Type Checking   │ ──► AST Valid / Type
└──────────────────┘
           │
           ▼
     ┌───────────┐
     │    HIR    │ ──► HIR / IR High Level (HIR.hs)
     └───────────┘
           │
           ▼
┌──────────────────┐
│   OptPasses      │ ──► HIR Optimized (OptPasses.hs)
└──────────────────┘
           │
           ▼
 [     Out code     ]
```
