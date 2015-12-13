This is Nonlinear regression analysis program. This explores functions including nonlinear (e.g. sqrt, log, exp, and so on) by genetic algorithm
for complicated model such as y = x1 + e^(72*x3)+x4^2+sqrt(x5)+1/x6

How to use: see src/main/scala/example package


To import this project, you need to clone and write following in build.sbt

    lazy val spamFilter = RootProject(file("C:\\workspaces\\NonlinearRegressionFunctionSelectionByGeneticAlgorithm"))
    val main = Project(id = "YOUR_PROJECT", base =file(".")).dependsOn(spamFilter)

You need to change address you've cloned and YOUR_PROJECT to your project name in build.sbt

--------------------------------------------------------
これは非線形重回帰分析プログラムです。このプログラムはy = x1 + e^(72*x3)+x4^2+sqrt(x5)+1/x6のような複雑なモデルに対して
非線形関数（sqrt, log, expなど）も含めて遺伝的アルゴリズムを用いて探索することで解を見つけ出します。

使い方はsrc/main/scala/example パッケージをごらんください


このプロジェクトをインポートする場合クローンしてbuild.sbtに以下のように記述します。

    lazy val spamFilter = RootProject(file("C:\\workspaces\\NonlinearRegressionFunctionSelectionByGeneticAlgorithm"))
    val main = Project(id = "YOUR_PROJECT", base =file(".")).dependsOn(spamFilter)

クローンした場所は自分の環境に合わせ、YOUR_PROJECTの部分は自分のプロジェクトのsbtファイルに記載されているnameに変えてください。
