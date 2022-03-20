# BLC
Haskell で書かれた [Binary lambda calculus](https://esolangs.org/wiki/Binary_lambda_calculus) のインタプリタ

```README.md``` - これ  
```blc.hs``` - BLC のインタプリタ  
```blc8.hs``` - BLC8 (下記リンク参照) のインタプリタ

正しく動作する場合があることが示されています (```skk.blc```は SKI コンビネータ計算の **S K K** にあたる, 入力をそのまま返すプログラム) :
```console
$ cat skk.blc
01 01 00000001011110100111010 0000110 0000110
$ cat test.in
Hello, world!
$ runghc blc.hs skk.blc < test.in
Hello, world!
```

## BLC ってなに
[ここ](https://tromp.github.io/cl/Binary_lambda_calculus.html) などが詳しい　まともなインタプリタも置いてある → https://tromp.github.io/cl/cl.html
