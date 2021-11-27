; ModuleID = 'main'


 


define external ccc  {i8* (i8*, i8*)*, {}*}* @$Show$Number$show()    {
  ret {i8* (i8*, i8*)*, {}*}* @showNumber 
}


@$Show$Number =    global {{i8* (i8*, i8*)*, {}*}* ()*} { {i8* (i8*, i8*)*, {}*}* ()* @$Show$Number$show }


@$Show$Boolean$show =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @$Show$Boolean$show$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @$Show$Boolean$show$fn(i8*  %env_0, i8*  %a_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %a_0 to i1* 
  %3 = load  i1, i1* %2, align 8 
  %4 =  call ccc  i8*  @GC_malloc(i64  13)  
  %5 = getelementptr  i8, i8* %4, i32 0 
  store  i8 115, i8* %5, align 8 
  %6 = getelementptr  i8, i8* %4, i32 1 
  store  i8 111, i8* %6, align 8 
  %7 = getelementptr  i8, i8* %4, i32 2 
  store  i8 109, i8* %7, align 8 
  %8 = getelementptr  i8, i8* %4, i32 3 
  store  i8 101, i8* %8, align 8 
  %9 = getelementptr  i8, i8* %4, i32 4 
  store  i8 32, i8* %9, align 8 
  %10 = getelementptr  i8, i8* %4, i32 5 
  store  i8 98, i8* %10, align 8 
  %11 = getelementptr  i8, i8* %4, i32 6 
  store  i8 111, i8* %11, align 8 
  %12 = getelementptr  i8, i8* %4, i32 7 
  store  i8 111, i8* %12, align 8 
  %13 = getelementptr  i8, i8* %4, i32 8 
  store  i8 108, i8* %13, align 8 
  %14 = getelementptr  i8, i8* %4, i32 9 
  store  i8 101, i8* %14, align 8 
  %15 = getelementptr  i8, i8* %4, i32 10 
  store  i8 97, i8* %15, align 8 
  %16 = getelementptr  i8, i8* %4, i32 11 
  store  i8 110, i8* %16, align 8 
  %17 = getelementptr  i8, i8* %4, i32 12 
  store  i8 0, i8* %17, align 8 
  ret i8* %4 
}


@$Show$Boolean =    global {{i8* (i8*, i8*)*, {}*}*} { {i8* (i8*, i8*)*, {}*}* @$Show$Boolean$show }


declare external ccc  i1 @puts(i8*)    


define external ccc  i8* @log$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %arg_0 to i8* 
  %3 =  call ccc  i1  @puts(i8*  %2)  
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %5 = bitcast i8* %4 to i1* 
  store  i1 %3, i1* %5, align 8 
  %6 = bitcast i1* %5 to i8* 
  ret i8* %6 
}


@log =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @log$0, {}* zeroinitializer }


declare external ccc  i8* @__doubleToStr__(double)    


define external ccc  i8* @showNumber$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %arg_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 =  call ccc  i8*  @__doubleToStr__(double  %3)  
  ret i8* %4 
}


@showNumber =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @showNumber$0, {}* zeroinitializer }


declare external ccc  i8* @MadList_singleton(i8*)    


define external ccc  i8* @singleton$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %arg_0 to i8* 
  %3 =  call ccc  i8*  @MadList_singleton(i8*  %2)  
  ret i8* %3 
}


@singleton =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @singleton$0, {}* zeroinitializer }


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


@$EMPTY_ENV =    global {} {  }


define external ccc  void @main()    {
entry_0:
  %0 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  %1 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %0, align 8 
  %2 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %1(double  2.100000e1)  
  %3 = bitcast {i8* (i8*, i8*)*, {}*}* %2 to i8* 
  %4 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %5 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %4, i32 0, i32 0 
  %6 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %5, align 8 
  %7 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %4, i32 0, i32 1 
  %8 = load  i8*, i8** %7, align 8 
  %9 =  call ccc  i8*  %6(i8*  %8, i8*  %3)  
  ret void 
}