# BLC
Haskell で書かれた [inary lambda calculus](https://esolangs.org/wiki/Binary_lambda_calculus) のインタプリタ

`README.md` - これ  
`blc.hs` - BLC のインタプリタ  
`blc8.hs` - BLC8 (下記リンク参照) のインタプリタ

正しく動作する場合があることが示されています (`skk.blc`は SKI コンビネータ計算の **S K K** にあたる, 入力をそのまま返すプログラム) :
```console
$ cat skk.blc
01 01 00000001011110100111010 0000110 0000110
$ cat test.in
Hello, world!
$ runghc blc.hs skk.blc < test.in
Hello, world!
```

## BLC ってなに
[ラムダ計算](https://ja.wikipedia.org/wiki/%E3%83%A9%E3%83%A0%E3%83%80%E8%A8%88%E7%AE%97) を`0`と`1`で書きあらわしたもの

[ここ](https://tromp.github.io/cl/Binary_lambda_calculus.html) などが詳しい　まともなインタプリタも置いてある → [tromp.github.io/cl/cl.html](https://tromp.github.io/cl/cl.html)
