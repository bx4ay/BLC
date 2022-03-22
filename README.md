# BLC
Haskell で書かれた [binary lambda calculus](https://esolangs.org/wiki/Binary_lambda_calculus) のインタプリタ

`README.md` - これ  
`blc.hs` - BLC のインタプリタ  
`blc8.hs` - BLC8 のインタプリタ (ソースコードは`0`と`1`からなる文字列で入力してください)

正しく動作する場合があることが示されています：
```console
$ cat skk.blc
01  01  00 00 00 01 01 1110 10 01 110 10  00 00 110  00 00 110
$ cat test.in
Hello, world!
$ runghc blc.hs skk.blc < test.in
Hello, world!
```
`skk.blc`は SKI コンビネータ計算の **S K K** にあたる，入力をそのまま返すプログラムです．`0`と`1`以外の文字は無視されます．

できること：
- β-簡約
- 遅延評価 (たぶん)

できないこと：
- η-変換
- エラーを吐くこと

## BLC ってなに
[ラムダ計算](https://ja.wikipedia.org/wiki/%E3%83%A9%E3%83%A0%E3%83%80%E8%A8%88%E7%AE%97) を`0`と`1`で書きあらわしたもの．

詳しくは [tromp.github.io/cl/](https://tromp.github.io/cl/cl.html) に [説明](https://tromp.github.io/cl/Binary_lambda_calculus.html) があります．
