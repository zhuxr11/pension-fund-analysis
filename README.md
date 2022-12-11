基金查看器 (Fund viewer)
================

## 简介 (Introduction)

2022年11月25日，中国的个人养老金正式落地。随之而来的是可供购买的上百个养老基金的Y份额。如何量化地挑选这些基金？本代码库就这个问题进行了初步的探索。

On Nov. 25th, 2022, personal pension was formally put forward in China.
Following this was the Y share of hundred of pension funds. How to
choose wisely from these pension funds in a quantitative way? This
repository provided a preliminary exploration.

## 用法 (Implementation)

系统要求：R (\>= 4.0.0), RStudio

Requirements: R (\>= 4.0.0), RStudio

首先，在RStudio中打开工程文件，运行`install.packages("renv")`在RStudio中安装`renv`软件包，并运行`renv::activate()`
和`renv::restore()`以确保下载并安装正确版本的软件包。

First, open project file in RStudio. Run `install.packages("renv")` to
install `renv` package. Run `renv::activate()` and `renv::restore()` to
ensure the project has the right versions of packages downloaded and
installed.

接着，在`lookup`文件夹中放入待处理的基金信息，例如从网页上复制的文本
(示例：
`lookup/pension_funds_20221203.txt`)，并运行`metadata.R`生成`data.frame`作为基因元数据，其行名
(必需)
为抓取历史数据用的基金代号。你可能需要调整代码以适应不同的输入格式。

Then, put into the `lookup` folder the fund information (e.g. text
copied from websites). Check for example:
`lookup/pension_funds_20221203.txt`. Run `metadata.R` to generate a
`data.frame` as fund metadata with row names (required) to get fund
history data. You may need to twitch the code to fit different input
formats.

然后，运行`script.R`抓取基金历史数据和计算指标。脚本使用基金元数据的行名作为代号进行历史数据的抓取。你可以调整代码以增加或替换需要计算的基金指标和时间段。

Next, run `script.R` to scrape funds’ history data and compute metrics.
The script uses the row names of the metadata as tokens to scrape the
history data. You can twitch the code to add or replace the computed
metrics or time periods.

最后，运行`app/fund_viewer/app.R`。这是一个Shiny应用程序。在`Fund statistics`页面，你可以选择不同的维度来查看不同情境下的基金指标。你还可以在`Fund metadata`页面查看基金元数据。页面中的所有表格均支持查找、过滤、选择和导出
(剪贴板、CSV、Excel和PDF)。

Finally, run `app/fund_viewer/app.R`. This is a Shiny application. In
the `Fund statistics` tab, you can select different dimensions to view
fund metrics under different circumstances. You can also view fund
metadata in the `Fund metadata` tab. Search, filtration, selection and
export (clipboard, CSV, Excel and PDF) are supported in all tables in
the tabs.

![](/app/fund_viewer/screenshot.png?raw=true)

## 声明 (Disclosure)

本代码库的所有基金数据均从因特网上获得，计算指标仅供参考，在法律允许的范围内不构成任何投资建议。本代码库作者不保证数据和指标的准确性及合理性。本代码库所有内容仅供学习交流，禁止用于商业化用途。如中英文两版内容有出入，以英文版内容为准。使用本代码库内容的任何人均不可撤回地同意上述声明。

All fund information and history data in this repository were obtained
from the Internet. The computed metrics are for reference only. Neither
data nor metrics constitute any form of investment suggestions under any
circumstances, to the extent permitted by applicable law. The author of
this repository do not guarantee the accuracy and rationality of the
data and the metrics. All content in this repository is used solely for
study and discussion, and no commercial use is permitted. If content
differs between the Chinese and English versions, please refer to the
English version for accuracy. Any who uses the content in this
repository irrevocably agrees with the aforementioned disclosure.
