# BLC
Haskell で書かれた [binary lambda calculus](https://esolangs.org/wiki/Binary_lambda_calculus) のインタプリタ

`README.md` - これ  
`blc.hs` - BLC のインタプリタ (ソースコードと入出力は`0`と`1`からなる文字列)  
`blc8.hs` - BLC8 のインタプリタ (ソースコードは`0`と`1`からなる文字列、入出力は任意の文字列)

「`0`と`1`からなる文字列」に含まれるその他の文字は無視されます：
```console
$ cat skk.blc
01 01 00000001011110100111010 0000110 0000110
$ cat test.in
Hello, world!
$ runghc blc8.hs skk.blc < test.in
Hello, world!
```
(`skk.blc`は SKI コンビネータ計算の **S K K** にあたる、入力をそのまま返すプログラム)

できること：
- β-簡約
- 遅延評価 (たぶん)

できないこと：
- η-変換
- エラーを吐くこと
- 無限に出力すること

## BLC ってなに
型なしラムダ計算をバイナリにエンコードしたもの。

詳しくは [tromp.github.io/cl/](https://tromp.github.io/cl/cl.html) に [説明](https://tromp.github.io/cl/Binary_lambda_calculus.html) があります。
