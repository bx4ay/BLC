# BLC
Haskell で書かれた [binary lambda calculus](https://tromp.github.io/cl/Binary_lambda_calculus.html) のインタプリタ

`README.md` - これ  
`blc.hs` - BLC のインタプリタ  
`blc8.hs` - BLC8 のインタプリタ

## BLC ってなに
型なしラムダ計算を [de Bruijn インデックス](https://ja.wikipedia.org/wiki/%E3%83%89%E3%83%BB%E3%83%96%E3%83%A9%E3%82%A6%E3%83%B3%E3%83%BB%E3%82%A4%E3%83%B3%E3%83%87%E3%83%83%E3%82%AF%E3%82%B9) 記法で書いてバイナリにエンコードしたものです。

&emsp;\[ *λ* expr \] = `00`\[ expr \]  
&emsp;\[ expr<sub>1</sub> expr<sub>2</sub> \] = `01`\[ expr<sub>1</sub> \]\[ expr<sub>2</sub> \]  
&emsp;\[ *i* \] = `1`<sup>*i*</sup> `0`

プログラムは入力を受け取り、出力を返す関数として扱われます。

BLC では、文字 (`0`か`1`) は Church ブーリアン、文字列はそれらの Church リストとしてエンコードされます。

BLC8 では、文字は Church ブーリアンの Church リスト、文字列はそれらの Church リストとしてエンコードされます。

## インタプリタについて
```
$ blc[8] [-b] ( -e "program-code" | program-file.blc )*
```
- 入力は標準入力から読み込まれ、出力は標準出力に書き込まれます。
- BLC のソースコードと入力、BLC8 のソースコードに含まれる`01`以外の文字は無視されます。
- `-b`オプションの有無は、標準入出力におけるバイナリモードのオン / オフを表します。
- `-e`の直後の引数は BLC / BLC8 のコードとして、そうでない引数はコードが入ったファイルの名称として解釈されます。
- プログラムを引数として与えなかった場合、標準入力の最初の 1 行がプログラムとして読み込まれます。
- 複数のプログラムを引数として与えた場合、それらは逆向きに関数合成されます。つまり、`blc prog1 prog2`は`blc prog1 | blc prog2`と同じような意味になります。

### できないこと
- η-変換
- 適切なエラーを出力すること
