# neo-diredy

標準Diredを、親ディレクトリ・現在の一覧・プレビューの3カラムで表示するEmacsパッケージです。Dirvish、Evil、Dired Previewは必須ではありません。

```text
Parent             ~/project/                    Preview
  docs/              src/                        選択したファイルの内容
▸ project/         ▸ README.md                    またはディレクトリ一覧
  tools/             LICENSE
```

## 導入

Emacs 29.1以降が必要です。DiredはEmacs標準の同梱ライブラリで、読み込み時に `require` します。別途インストールする依存パッケージはありません。検証環境はWindows / Emacs 31.1です。

ローカルのチェックアウトを使う場合は、設定に次を追加します。

```elisp
(add-to-list 'load-path "C:/path/to/neo-diredy")
(with-eval-after-load 'dired
  (require 'neo-diredy))
```

この設定で、Diredを開くと自動的に3カラム表示になります。パッケージを読み込むだけで `neo-diredy-mode` が有効になります。毎回コマンドを実行する必要はありません。

GitHubに公開した後は、`M-x package-vc-install` に次のURLを指定して導入することもできます。

```text
https://github.com/siroio/neo-diredy
```

その場合も、設定に `(with-eval-after-load 'dired (require 'neo-diredy))` を追加してください。`M-x package-install-file` で `neo-diredy.el` を直接導入する方法も使えます。

旧 `neo-dired-yazi-mode` とは併用しないでください。移行前に旧モードを無効にし、旧モードの自動有効化設定がある場合は取り除いてください。

## 操作と表示

- 中央は本物のDiredです。移動、マーク、コピー、リネーム、削除、再読込など、標準Diredの操作をそのまま使えます。Evilを使用している場合も、既存のDired用キー設定を使います。
- `M-x neo-diredy-mode` で通常のDired表示へ戻せます。同じコマンドで再び有効にできます。
- ディレクトリ移動時は、次の画面描画までに表示を整えます。カーソル移動のプレビューだけ0.12秒待って更新をまとめます。
- ファイルを開くと補助欄を閉じます。タブを切り替える前にも閉じ、空の補助欄を保存タブに残しません。
- 一度に表示する3カラムのレイアウトは1つです。幅72桁未満のウィンドウとリモートディレクトリでは通常のDiredを使います。
- 配色は現在のテーマを継承します。選択行は `neo-diredy-selection` faceで調整できます。

## プレビュー

プレビューは専用バッファを使い、編集中のファイルや未保存の変更を再利用・破棄しません。ファイルのローカル変数や編集用フックも実行しません。テキストの構文ハイライトや外部形式の変換は行いません。

| 項目 | 既定値 |
| --- | --- |
| テキスト読込上限 | 64 KiB (`neo-diredy-preview-bytes`) |
| 画像ファイル上限 | 8 MiB (`neo-diredy-image-bytes`) |
| 補助欄のディレクトリ一覧 | 最大500件 |

画像はEmacsが対応している形式を使用します。大きい画像、未対応形式、バイナリファイルでは概要を表示します。同じファイルの再表示は、更新日時・サイズ・表示寸法などが変わらない間は読み直しません。外部で更新した内容を明示的に取り直すにはDiredの再読込 (`g`) を使います。

## 検証と性能

パッケージのディレクトリで実行します。ユーザーのinitや既存セッションは読み込みません。

```powershell
emacs -Q --batch -l tests/neo-diredy-tests.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -l tests/check-package.el
emacs -Q --batch -l benchmarks/run.el
```

テスト用データは `tests/` 内、計測用データは `.bench/` 内に一時作成し、実行後にその実行で作ったディレクトリだけを削除します。計測では2,100件の一覧を使います。隣に `emacs.d/lisp/` がある場合は、その実際のソース一覧も読み取りのみで計測します。

計測結果と改善内容は [benchmarks/RESULTS.md](benchmarks/RESULTS.md) を参照してください。バッチ計測は描画時間や実キー入力の遅延を含みません。

## ライセンス

GPL-3.0-or-later。全文は [LICENSE](LICENSE) を参照してください。
