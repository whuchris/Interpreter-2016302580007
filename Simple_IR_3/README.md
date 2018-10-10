# InterpreterProject-2016302580007
A project about interpreter at 2016302580007
* 陈开一 2016302580007 (代码Githup部分)
* 南思宇 2016302580017 (设计文档)
* 史雪峰 2016302580019 (设计文档)
* 张凯康 2016302580020 (设计文档)
* 叶晟柯 2016302160017 (设计文档)

利用LLVM的Module容器获取代码对象，生成简单的中间代码。

### 一.抽象语法树的构造完善
在原有的LLVMchapter2基础上补全抽象语法树AST。采用虚函数codegen()从而获取各个节点的信息。这里我们创建一个新的虚函数，showSrc()。该函数用来获取各个节点的内容并且转换为string类型的对象返回，这样我们就可以打印原始代码形式：

![image text](https://github.com/Bluchris/Expression-2016302580007/blob/master/Simple_IR_3/virtual_method.png)

我们在后面实现showSrc()函数，用于之后打印每一个节点的内容：

![image text](https://github.com/Bluchris/Expression-2016302580007/blob/master/Simple_IR_3/method_acc.png)

### 二.函数体的获取和生成
在 FunctionAST类中的codegen()中间代码生成函数中，首先我们从module这个顶级容器中获取我们需要的函数名称，FunctionAST中包含了函数声明和函数体，我们首先在Proto->codegen()中检测函数是否已经声明，如果函数未声明则直接采用logerror函数返回错误日志:
![image text](https://github.com/Bluchris/Expression-2016302580007/blob/master/Simple_IR_3/log_error.png)

其次检查函数体是否为空，如果为空则也返回错误日志。如果一切正常则我们返回获取函数体并解析，创建返回值打印ret：
![image text](https://github.com/Bluchris/Expression-2016302580007/blob/master/Simple_IR_3/FunctionAST.png)

### 三.结果测试
#### 输出表达式的中间代码:
![image text](https://github.com/Bluchris/Expression-2016302580007/blob/master/Simple_IR_3/expr_result.png)

#### 输出函数和函数体的中间代码:
![image text](https://github.com/Bluchris/Expression-2016302580007/blob/master/Simple_IR_3/func_result.png)

#### 输出函数重定义的错误日志:
![image text](https://github.com/Bluchris/Expression-2016302580007/blob/master/Simple_IR_3/redef_result.png)
