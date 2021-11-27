; ModuleID = 'main'


 


define external ccc  i8* @Just(i8* )    {
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i8*}* getelementptr inbounds ({i64, i8*}, {i64, i8*}* inttoptr (i32 0 to {i64, i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i64, i8*}* 
  %4 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 0 
  store  i64 0, i64* %4, align 8 
  %5 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 1 
  store  i8* %0, i8** %5, align 8 
  %6 = bitcast {i64, i8*}* %3 to i8* 
  ret i8* %6 
}


@Nothing =    global i8* bitcast ({i64} { i64 1 } to i8)


define external ccc  i8* @id(i8*  %x_0)    {
entry_0:
  %0 = bitcast i8* %x_0 to i8* 
  ret i8* %0 
}


declare external ccc  i32 @puts(i8*)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


define external ccc  void @main()    {
entry_0:
  %0 = alloca double, align 8 
  store  double 3.000000e0, double* %0, align 8 
  %1 = bitcast double* %0 to i8* 
  %2 =  call ccc  i8*  @Just(i8*  %1)  
  %3 =  call ccc  i8*  @id(i8*  %2)  
  ret void 
}