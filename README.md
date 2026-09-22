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
     ┌───────────┐
     │    HIR    │ ──► HIR / IR High Level (HIR.hs)
     └───────────┘
           │
           ▼
┌──────────────────┐
│  Type Checking   │ ──► AST Valid / Type
└──────────────────┘
           │
           ▼
┌──────────────────┐
│   OptPasses      │ ──► HIR Optimized (OptPasses.hs)
└──────────────────┘
           │
           ▼
 [     Out code     ]
```
