# Wagon: 型安全性を持つSIMD命令を利用可能なDSL
<u>祁 全羽</u>, 亀山 幸義 (筑波大学)

## 目的

* 高レベル関数型言語を用いてままSIMD命令付き実行効率高いコードの生成
* 型安全性低いSIMD組込み関数の不適切な利用を減らす

## SIMDとは？

* Single Instruction Multiple Dataの略称
* 一つの命令は複数のデータを扱える
* クロックサイクル数はスカラー命令と似ている
* x86系・組込みシステムなど多数のアーキテクチャが搭載

## 問題点

* コンパイラによる自動ベクトル化機能は完成度低い
  * 手動ベクトル化（一部でも）はより高い実行効率は可能
* C言語のSIMD組込み関数は型検査が非常に弱い
  * 整数SIMDベクトルは整数のビット数情報を持っていない
  * SIMD組込み関数の引数はメモリアラインメントが正しくなくてもコンパイルできる
    →実行時エラーになる
* SIMD組込み関数を使うC言語自体は暗黙的型変換が多い
* 手動でSIMD組込み関数を用いての最適化はデバッグ困難

## 先行研究[1] | HaskellのSIMDサポート

* 効率的なメモリアラインメントの実現は完成度低い
  * 128bitベクトルでは128bitにアラインメントがなくてもメモリーロード可能
  * 256bit以上のベクトル命令は特殊アラインメント必要
* 128bitアラインメント（高効率）に対応するのはpinned arrayのみ
* スタック・ヒープ領域データのアラインメントは未実装

## 先行研究[2] | Accelerate

* 配列プログラミング言語
* LLVM/CUDAバックエンド
* HaskellのDSL
* 実行時コンパイル？→要調査
* 未完成課題？→要調査

## Tagless-finalアプローチ

* ホスト言語の型システムによる最大限活用例
  * 最小限のコード量
  * 拡張は数箇所の書き直し不要→不具合が少ない

## Wagonコードの例

```ocaml
let wfunc_example = ...
(* ベクトルの内積 *)
```

## 型依存？(+x)

## Next Version

* ポスターの容器(>=1) Container for posters
* Instructions for Large-scale Printer
  -> Mr. Takaki, Nanjo, Ohkura, Yuhi
* 大判プリンターの使い方
* deadline 2020/02/21 (draft)

## Wagonの文法

```ocaml
wtype := i8 pdt | i16 pdt | i32 pdt
struct := 
```

* PDT = Primitive Data Type基本型

## 実装の問題点

* 異種データをともに格納でき・再帰的にアクセスできるリスト(つまりC言語風`struct`)の実装法
  * OCamlでは`record`(C言語の`struct`相当)はイテレータは無い
  * OCamlの`tuple`は二つ要素持つによる`fst`(first)と`snd`(second)しかない
  * OCamlでは型名・変数名の名前空間は共通ではない
  * OCamlはタイプクラス相当な実装もない

## 上記問題点に対する提案

* 再帰な`tuple`構造に対する番号付き取得関数の自動生成機能を実現する最小PPX拡張
  * `[%hlistelem 5]` -> `fun (_, (_, (_, (_, (_, (a, _)))))) -> a`

* PDT := int8 | int16 | int32 | int64 | float32 | float64