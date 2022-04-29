# GucciLang Syntax

```mermaid
stateDiagram-v2
  state Module {
    direction LR
    [*] --> Id: module
    Id --> Block
    Block --> [*]
}
```

```mermaid
  stateDiagram-v2
    state Block {
      direction LR
      [*] --> Statement : {
      [*] --> ReturnStatement : {
      Statement --> ReturnStatement
      Statement --> Statement: SEMICOLON
      ReturnStatement --> [*]: }
    }
```

```mermaid
stateDiagram-v2
  state ReturnStatement {
    direction LR
    [*] --> [*]
    [*] --> Expression: return
    Expression --> [*]: SEMICOLON
    Expression --> [*]
  }
```

```mermaid
stateDiagram-v2
state Statement {
  direction LR
  [*] --> Declaration
  Declaration --> [*]
  [*] --> Assignment
  Assignment --> [*]
  [*] --> Expression
  Expression --> [*]
  [*] --> Condition
  Condition --> [*]
  [*] --> Loop
  Loop --> [*]
}
```


```mermaid
stateDiagram-v2
  state Assignment {
    direction LR
    [*] --> Id
    Id --> Expression: =
    Expression --> [*]
  }
```

```mermaid
stateDiagram-v2
  state Condition {
    direction LR
    [*] --> Expression: if
    Expression --> Block
    Block --> [*]
    Block --> Expression: else if
    Block --> <Else>Block: else
    <Else>Block --> [*]
  }
```

```mermaid
stateDiagram-v2
  state Loop {
    direction LR
    [*] --> Expression: while
    Expression --> Block
    Block --> [*]
  }
```

```mermaid
stateDiagram-v2
  state Declaration {
    direction LR
    [*] --> VariableDeclaration
    VariableDeclaration --> [*]

    [*] --> FunctionDeclaration
    FunctionDeclaration --> [*]

    [*] --> ClassDeclaration
    ClassDeclaration --> [*]

    [*] --> InterfaceDeclaration
    InterfaceDeclaration --> [*]
  }
```

```mermaid
stateDiagram-v2
  state VariableDeclaration {
    direction LR
    [*] --> Id: let
    Id --> Type: COLON
    Type --> [*]
    
    Type --> Expression: =
    Expression --> [*]
  }
```

```mermaid
stateDiagram-v2
  state Type {
    direction LR
    [*] --> PrimitiveType
    PrimitiveType --> [*]
    [*] --> CompoundType
    CompoundType --> [*]
    [*] --> CustomType
    CustomType --> [*]
  }
```

```mermaid
stateDiagram-v2
  state FunctionDeclaration {
    direction LR
    [*] --> Id: fun
    Id --> Arguments: (
    Arguments --> Type: ) COLON
    Type --> Block
    Block --> [*]
  }
```

```mermaid
stateDiagram-v2
  state Arguments {
    direction LR
    [*] --> [*]
    [*] -->  ArgumentDeclaration
    ArgumentDeclaration --> ArgumentDeclaration: ,
    ArgumentDeclaration --> [*]
  }
```

```mermaid
stateDiagram-v2
  state ArgumentDeclaration {
    direction LR
    [*] -->  Id
    Id --> Type: COLON
    Type --> [*]
  }
```

```mermaid
stateDiagram-v2
  state ClassDeclaration {
    direction LR
    [*] -->  Id: class
    Id --> Block
    Block --> [*]
  }
```

```mermaid
stateDiagram-v2
  state InterfaceDeclaration {
    direction LR
    [*] -->  Id: interface
    Id --> Block
    Block --> [*]
  }
```


```mermaid
stateDiagram-v2
state Expression {
  direction LR
  [*] --> AndExp
  AndExp --> [*]
  AndExp --> AndExp: ||
}
```

```mermaid
stateDiagram-v2
state AndExp {
  direction LR
  [*] --> CmpExp
  CmpExp --> [*]
  CmpExp --> CmpExp: &&
}
```

```mermaid
stateDiagram-v2
state CmpExp {
  direction LR
  [*] --> <left>TermExp
  <left>TermExp --> [*]
  <left>TermExp --> CmpOp
  CmpOp --> <right>TermExp
  <right>TermExp --> [*]
}
```

```mermaid
stateDiagram-v2
state TermExp {
  direction LR
  [*] --> UnaryOp
  UnaryOp --> Term
  [*] --> Term
  Term --> [*]
  Term --> TermOp
  TermOp --> Term
}
```

```mermaid
stateDiagram-v2
state Term {
  direction LR
  [*] --> ChainExp
  ChainExp --> [*]
  ChainExp --> FacOp
  FacOp --> ChainExp
}
```

```mermaid
stateDiagram-v2
state Factor {
  direction LR
  [*] --> Expression: (
  Expression --> [*]: )
  [*] --> Constant
  Constant --> [*]
  [*] --> Id
  Id --> [*]
  [*] --> FunctionCall
  FunctionCall --> [*]
  [*] --> ChainExp
  ChainExp --> [*]
}
```

```mermaid
stateDiagram-v2
state FunctionCall {
  direction LR
  [*] --> Id
  Id --> (
  ( --> Expression
  ( --> )
  Expression --> Expression: ,
  Expression --> )
  ) --> [*]
}
```

```mermaid
stateDiagram-v2
state ChainExp {
  direction LR
  [*] --> Chain
  Chain --> [*]
  Chain --> Chain: .
}
```

```mermaid
stateDiagram-v2
state Chain {
  direction LR
  [*] --> Id
  Id --> [*]
  [*] --> FunctionCall
  FunctionCall --> [*]
}
```

```mermaid
stateDiagram-v2
state Constant {
  direction LR
  [*] --> Int
  Int --> [*] 
  [*] --> Float
  Float --> [*]
  [*] --> Bool
  Bool --> [*]
  [*] --> Char
  Char --> [*]
  [*] --> String
  String --> [*]
}
```

```mermaid
stateDiagram-v2
state CmpOp {
  direction LR
  [*] --> [*]: >
  [*] --> [*]: <
  [*] --> [*]: ==
  [*] --> [*]: >=
  [*] --> [*]: <=
  [*] --> [*]: !=
}
```

```mermaid
stateDiagram-v2
state TermOp {
  direction LR
  [*] --> [*]: +
  [*] --> [*]: -
}
```

```mermaid
stateDiagram-v2
state FacOp {
  direction LR
  [*] --> [*]: *
  [*] --> [*]: /
}
```

```mermaid
stateDiagram-v2
state UnaryOp {
  direction LR
  [*] --> [*]: !
  [*] --> [*]: -
}
```

```mermaid
stateDiagram-v2
state PrimitiveType {
  direction LR
  [*] --> [*]: int
  [*] --> [*]: float
  [*] --> [*]: bool
  [*] --> [*]: char
}
```

```mermaid
stateDiagram-v2
state CompoundType {
  direction LR
  [*] --> TupleType
  TupleType --> [*]
  [*] --> ArrayType
  ArrayType --> [*]
}
```

```mermaid
stateDiagram-v2
state TupleType {
  direction LR
  [*] --> Type: (
  Type --> [*]: )
  Type --> Type: ,
}
```

```mermaid
stateDiagram-v2
state ArrayType {
  direction LR
  [*] --> Type: [
  Type --> [*]: ]
}
```

```mermaid
stateDiagram-v2
state PrimitiveType {
  direction LR
  [*] --> [*]: int
  [*] --> [*]: float
  [*] --> [*]: bool
  [*] --> [*]: char
}
```