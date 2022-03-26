# BLC
Haskell で書かれた [binary lambda calculus](https://esolangs.org/wiki/Binary_lambda_calculus) のインタプリタ

`README.md` - これ  
`blc.hs` - BLC のインタプリタ  
`blc8.hs` - BLC8 のインタプリタ

## BLC ってなに
型なしラムダ計算を [de Bruijn インデックス](https://ja.wikipedia.org/wiki/%E3%83%89%E3%83%BB%E3%83%96%E3%83%A9%E3%82%A6%E3%83%B3%E3%83%BB%E3%82%A4%E3%83%B3%E3%83%87%E3%83%83%E3%82%AF%E3%82%B9) 記法で書いてバイナリにエンコードしたもの。

&emsp;\[ *λ* expr \] = `00` \[ expr \]  
&emsp;\[ expr<sub>1</sub> expr<sub>2</sub> \] = `01` \[ expr<sub>1</sub> \] \[ expr<sub>2</sub> \]  
&emsp;\[ *i* \] = `1`<sup>*i*</sup> `0`

プログラムは入力を受け取り、出力を返す関数として扱われます。

BLC では、文字 (`0`か`1`) は Church ブーリアン、文字列はそれらの Church リストとしてエンコードされます。

BLC8 では、文字は Church ブーリアンの Church リスト、文字列はそれらの Church リストとしてエンコードされます。

詳しくは：
- [John's Lambda Calculus and Combinatory Logic Playground](https://tromp.github.io/cl/cl.html)
- [Binary lambda calculus](https://tromp.github.io/cl/Binary_lambda_calculus.html)

## インタプリタについて
例：
```console
$ ghc blc8.hs
$ cat skk.blc
01 01 00000001011110100111010 0000110 0000110
$ cat test.in
Hello, world!
$ ./blc8 skk.blc < test.in
Hello, world!
```
(`skk.blc`は SKI コンビネータ計算の **S K K** にあたる、入力をそのまま返すプログラム)
- 入力は標準入力から読み込まれ、出力は標準出力に書き込まれます。
- BLC のソースコードと入力、BLC8 のソースコードに含まれる`0`と`1`以外の文字は無視されます。
- `[program-file].blc`の代わりに`-e "[program-code]"`と記述すれば、`[program-code]`自体が BLC / BLC8 のコードとして解釈・実行されます。
- 複数のプログラムを引数として与えた場合、それらは逆向きに関数合成されます。つまり、`blc code1 code2`は`blc code1 | blc code2`のような意味になります。

### できること
- β-簡約
- 遅延評価

### できないこと
- η-変換
- 適切なエラーを吐くこと
