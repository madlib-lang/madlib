; ModuleID = 'main'


 


define external ccc  i8* @__closure__0(i8*  %env_0, i8*  %c_0)    {
  %1 = bitcast i8* %env_0 to {i8*}* 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = bitcast i8* %3 to i8* 
  %5 = bitcast i8* %c_0 to i8* 
  ret i8* %4 
}


define external ccc  i8* @__closure__1(i8*  %env_0, i8*  %b_0)    {
  %1 = bitcast i8* %env_0 to {i8*}* 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = bitcast i8* %3 to i8* 
  %5 = bitcast i8* %b_0 to i8* 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i8*}* 
  %8 = getelementptr  {i8*}, {i8*}* %7, i32 0, i32 0 
  store  i8* %4, i8** %8, align 8 
  %9 = load  {i8*}, {i8*}* %7, align 8 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*}}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*}}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i8* (i8*, i8*)*, {i8*}}* 
  %12 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %11, i32 0, i32 0 
  store  i8* (i8*, i8*)* @__closure__0, i8* (i8*, i8*)** %12, align 8 
  %13 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %11, i32 0, i32 1 
  store  {i8*} %9, {i8*}* %13, align 8 
  %14 = bitcast {i8* (i8*, i8*)*, {i8*}}* %11 to i8* 
  ret i8* %14 
}


define external ccc  i8* @snd$fn(i8*  %env_0, i8*  %a_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %a_0 to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*}* 
  %5 = getelementptr  {i8*}, {i8*}* %4, i32 0, i32 0 
  store  i8* %2, i8** %5, align 8 
  %6 = load  {i8*}, {i8*}* %4, align 8 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*}}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*}}*), i32 1) to i64))  
  %8 = bitcast i8* %7 to {i8* (i8*, i8*)*, {i8*}}* 
  %9 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %8, i32 0, i32 0 
  store  i8* (i8*, i8*)* @__closure__1, i8* (i8*, i8*)** %9, align 8 
  %10 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %8, i32 0, i32 1 
  store  {i8*} %6, {i8*}* %10, align 8 
  %11 = bitcast {i8* (i8*, i8*)*, {i8*}}* %8 to i8* 
  ret i8* %11 
}


@snd =    global {i8* (i8*, i8*)*, {}} { i8* (i8*, i8*)* @snd$fn, {} @$EMPTY_ENV }


declare external ccc  i32 @puts(i8*)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


@$EMPTY_ENV =    global {} {  }


define external ccc  void @main()    {
entry_0:
  ret void 
}