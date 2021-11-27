; ModuleID = 'main'


 


define external ccc  i8* @$Show$Number$show(i8*  %a_0)    {
entry_0:
  %0 = ptrtoint i8* %a_0 to i64 
  %1 = bitcast i64 %0 to double 
  %2 =  call ccc  i8*  @GC_malloc(i64  12)  
  %3 = getelementptr  i8, i8* %2, i32 0 
  store  i8 115, i8* %3, align 8 
  %4 = getelementptr  i8, i8* %2, i32 1 
  store  i8 111, i8* %4, align 8 
  %5 = getelementptr  i8, i8* %2, i32 2 
  store  i8 109, i8* %5, align 8 
  %6 = getelementptr  i8, i8* %2, i32 3 
  store  i8 101, i8* %6, align 8 
  %7 = getelementptr  i8, i8* %2, i32 4 
  store  i8 32, i8* %7, align 8 
  %8 = getelementptr  i8, i8* %2, i32 5 
  store  i8 110, i8* %8, align 8 
  %9 = getelementptr  i8, i8* %2, i32 6 
  store  i8 117, i8* %9, align 8 
  %10 = getelementptr  i8, i8* %2, i32 7 
  store  i8 109, i8* %10, align 8 
  %11 = getelementptr  i8, i8* %2, i32 8 
  store  i8 98, i8* %11, align 8 
  %12 = getelementptr  i8, i8* %2, i32 9 
  store  i8 101, i8* %12, align 8 
  %13 = getelementptr  i8, i8* %2, i32 10 
  store  i8 114, i8* %13, align 8 
  %14 = getelementptr  i8, i8* %2, i32 11 
  store  i8 0, i8* %14, align 8 
  ret i8* %2 
}


@$Show$Number =    global {i8* (i8*)*} { i8* (i8*)* @$Show$Number$show }


define external ccc  i8* @$Show$Boolean$show(i8*  %a_0)    {
entry_0:
  %0 = ptrtoint i8* %a_0 to i1 
  %1 = bitcast i1 %0 to i1 
  %2 =  call ccc  i8*  @GC_malloc(i64  13)  
  %3 = getelementptr  i8, i8* %2, i32 0 
  store  i8 115, i8* %3, align 8 
  %4 = getelementptr  i8, i8* %2, i32 1 
  store  i8 111, i8* %4, align 8 
  %5 = getelementptr  i8, i8* %2, i32 2 
  store  i8 109, i8* %5, align 8 
  %6 = getelementptr  i8, i8* %2, i32 3 
  store  i8 101, i8* %6, align 8 
  %7 = getelementptr  i8, i8* %2, i32 4 
  store  i8 32, i8* %7, align 8 
  %8 = getelementptr  i8, i8* %2, i32 5 
  store  i8 98, i8* %8, align 8 
  %9 = getelementptr  i8, i8* %2, i32 6 
  store  i8 111, i8* %9, align 8 
  %10 = getelementptr  i8, i8* %2, i32 7 
  store  i8 111, i8* %10, align 8 
  %11 = getelementptr  i8, i8* %2, i32 8 
  store  i8 108, i8* %11, align 8 
  %12 = getelementptr  i8, i8* %2, i32 9 
  store  i8 101, i8* %12, align 8 
  %13 = getelementptr  i8, i8* %2, i32 10 
  store  i8 97, i8* %13, align 8 
  %14 = getelementptr  i8, i8* %2, i32 11 
  store  i8 110, i8* %14, align 8 
  %15 = getelementptr  i8, i8* %2, i32 12 
  store  i8 0, i8* %15, align 8 
  ret i8* %2 
}


@$Show$Boolean =    global {i8* (i8*)*} { i8* (i8*)* @$Show$Boolean$show }


declare external ccc  i32 @puts(i8*)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


define external ccc  void @main()    {
entry_0:
  %0 = getelementptr  {i8* (i8*)*}, {i8* (i8*)*}* @$Show$Number, i32 0, i32 0 
  %1 = load  i8* (i8*)*, i8* (i8*)** %0, align 8 
  %2 = alloca double, align 8 
  store  double 3.000000e0, double* %2, align 8 
  %3 = bitcast double* %2 to i8* 
  %4 =  call ccc  i8*  %1(i8*  %3)  
  %5 = getelementptr  {i8* (i8*)*}, {i8* (i8*)*}* @$Show$Boolean, i32 0, i32 0 
  %6 = load  i8* (i8*)*, i8* (i8*)** %5, align 8 
  %7 = bitcast i1 1 to i8* 
  %8 =  call ccc  i8*  %6(i8*  %7)  
  %9 =  call ccc  i32  @puts(i8*  %4)  
  %10 =  call ccc  i32  @puts(i8*  %8)  
  ret void 
}