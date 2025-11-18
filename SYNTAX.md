# Sliip — 現行構文仕様（簡潔版）

この文書は Sliip の現在の構文仕様を構文そのものだけに絞ってまとめたものです。型クラスや deriving は未導入です。関数は必ず lambda で作り、define は常に (define name expr) 形式のみを許容します。型注釈は (as expr Type) のみを公式表記として扱います。

## トークンとコメント
- S 式ベース：リストは丸括弧 `(...)`
- 行コメント: `;` から行末
- ブロックコメント: `#| ... |#`
- 文字列: `"..."`（エスケープあり）
- 真偽値: `true`, `false`
- 数値リテラル: 整数・浮動小数点

## リテラルと識別子
- 数値、文字列、真偽値をリテラルとして扱う。
- シンボル（識別子）は英数字と記号を含む。ただし予約語はフォームで扱う。

## 型注釈（as）
- 公式表記: `(as expr Type)`
  - 任意の式に付与可能。AST 上は Ascription ノードとして扱う。
  - 例: `(as 42 Int)`, `(as (+ x y) Int)`
- 便宜的糖（任意）:
  - 引数／束縛で短縮記法 `(name Type [init])` を許容し、パーサで `(name (as init Type))` に展開して扱える。ソースでは `(as ...)` を推奨。

## 型表現（最小）
- 単純型名: `Int`, `Bool`, `String`, `Unit` など
- 型変数: 大文字で表記 `T`, `U`, ...
- 型コンストラクタ（最小）:
  - `(List T)`
  - `(Option T)`
  - `(-> A B ...)`  ; 関数型（可変長、最後が返り値）
- 高度機能（typeclass, deriving 等）は未導入

## 主要フォーム（構文）
- define
  - 形式: `(define name expr)`
  - name に expr の評価結果を束縛する（トップレベル／局所共通の扱いは実装方針による）
  - 関数定義は必ず lambda を用いる:
    - 正しい: `(define f (lambda (x) body))`
    - 無効（非公式）: `(define (f x) body)` は採用しない

- lambda
  - 形式: `(lambda (arg1 arg2 ...) body1 body2 ...)`
  - 引数の型糖: `(lambda ((x Int) (y Int)) ...)` をパーサで許容して `(as ...)` に展開可能
  - 戻り型注釈は body に `(as ...)` を付与することで表現

- 関数適用
  - 形式: `(f a b ...)`

- let / let* / letrec
  - 形式:
    - `(let ((name expr) ...) body...)`
    - `(let* ((name expr) ...) body...)`
    - `(letrec ((name expr) ...) body...)`  ; 相互再帰を許可
  - 糖／型短縮: `(let ((x Int 1) ...))` を `x (as 1 Int)` に展開して扱える

- if
  - 形式: `(if cond then-expr else-expr)`  ; else は必須（初期簡潔性のため）

- begin
  - 形式: `(begin expr1 expr2 ...)` ; シーケンス

- quote
  - 形式: `'( ...)` または `(quote ...)`

- def-type（ADT）
  - 形式:
    ~~~
    (def-type TypeName (T1 T2 ...)
      (Ctor1 Ty11 Ty12 ...)
      (Ctor2 ...)
      ...)
    ~~~
  - フィールドは位置引数のみ。名前付きフィールド構文は無し。

- match（パターンマッチ）
  - 形式:
    ~~~
    (match expr
      (pattern1 expr1 ...)
      (pattern2 expr2 ...)
      ...)
    ~~~
  - サポートする最小パターン:
    - コンストラクタパターン: `(Ctor x y)`
    - リストパターン: `()`（空リスト）, `(cons h t)`
    - ワイルドカード: `_`
  - パターン内での型注釈は持たず、束縛後に `(as ...)` を用いることを推奨

## モジュール / インポート（最小）
- 形式例:
  - `(module Name body...)`
  - `(export sym1 sym2 ...)`
  - `(import Name :as Alias)` または `(import Name (sym1 sym2))`
- トップレベル define 群はモジュール単位でまとめて処理し、相互参照をサポートする実装が想定される

## マクロ（注）
- シンタックスマクロの導入は実装方針により選択可能だが、構文仕様自体はマクロ非必須で成り立つ

## 簡易 BNF（読みやすさ優先の最小文法）
~~~
program        ::= form*
form           ::= expr
expr           ::= atom
                 | list
atom           ::= NUMBER | STRING | SYMBOL | BOOL
list           ::= '(' elements ')'
elements       ::= /* empty */ | element+
element        ::= SYMBOL | expr

expr           ::= '(' 'define' SYMBOL expr ')'                      ; define
                 | '(' 'lambda' '(' params ')' body ')'             ; lambda
                 | '(' 'let' '(' binding* ')' body ')'              ; let
                 | '(' 'let*' '(' binding* ')' body ')'             ; let*
                 | '(' 'letrec' '(' binding* ')' body ')'           ; letrec
                 | '(' 'if' expr expr expr ')'                      ; if
                 | '(' 'begin' expr+ ')'                            ; begin
                 | '(' 'quote' expr ')'                             ; quote
                 | '(' 'def-type' SYMBOL '(' type-params ')' ctor+ ')' ; def-type
                 | '(' 'match' expr match-clause+ ')'                ; match
                 | '(' 'as' expr type ')'                           ; ascription
                 | '(' expr+ ')'                                    ; application

binding        ::= '(' SYMBOL expr ')'
params         ::= /* empty */ | param+
param          ::= SYMBOL | '(' SYMBOL type ')'   ; 型糖を許容
ctor           ::= '(' SYMBOL type* ')'
match-clause   ::= '(' pattern expr+ ')'
pattern        ::= SYMBOL | '(' SYMBOL pattern* ')' | '_' | '()' | '(' 'cons' SYMBOL SYMBOL ')'
type           ::= SYMBOL | '(' 'List' type ')' | '(' 'Option' type ')' | '(' '->' type+ ')'
type-params    ::= /* empty */ | SYMBOL+
~~~

## 最低限の実装上の要点
- 関数は常に `lambda` によって生成する。`(define (name args...) ...)` 形式は言語仕様に含めない。
- 型注釈は `(as expr Type)` を唯一の公式表現とする。引数／束縛の短縮糖は実装内部で `(as ...)` に展開する。
- `def-type` は位置引数のみのコンストラクタを持つ ADT を定義する。コンストラクタはタグ付き値として表現する。
- `match` はタグ判定で分岐し、束縛されたフィールドは分岐内で参照できるようにする。
- トップレベル定義の相互参照は `letrec` 相当の扱いでサポートすることが想定される。
