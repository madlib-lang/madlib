; ModuleID = 'main'


 


declare external ccc  i8* @__5dd8d8f67f6caa3e4c41a3cdcb44ab61__prettyLog(i8*)    


@$PrettyShow$Number = external   global {{i8*, i32, i32, i8*}}


declare external ccc  i8* @$PrettyShow$Number$ppShow(i8*)    


declare external ccc  i8* @__applyPAP__(i8*, i32, ...)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


declare external ccc  i8* @__strConcat__(i8*, i8*)    


declare external ccc  i1 @MadList_hasMinLength(double, {i8*, i8*}*)    


declare external ccc  i1 @MadList_hasLength(double, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_singleton(i8*)    


declare external ccc  {i8*, i8*}* @__MadList_push__(i8*, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_concat({i8*, i8*}*, {i8*, i8*}*)    


declare external ccc  void @__5dd8d8f67f6caa3e4c41a3cdcb44ab61__moduleFunction()    


define external ccc  void @main()    {
entry_0:
   call ccc  void  @__5dd8d8f67f6caa3e4c41a3cdcb44ab61__moduleFunction()  
  %0 = bitcast i8* (i8*)* @__5dd8d8f67f6caa3e4c41a3cdcb44ab61__prettyLog to i8* 
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i8*, i32, i32, i8*}* 
  %3 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 0 
  store  i8* %0, i8** %3, align 8 
  %4 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 1 
  store  i32 1, i32* %4, align 8 
  %5 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 2 
  store  i32 1, i32* %5, align 8 
  %6 = bitcast {i8*, i32, i32, i8*}* %2 to i8* 
  %7 = bitcast {{i8*, i32, i32, i8*}}* @$PrettyShow$Number to i8* 
  %8 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %6, i32  1, i8*  %7)  
  %9 = bitcast i8* %8 to {i8*, i32, i32, i8*}* 
  %10 = bitcast {i8*, i32, i32, i8*}* %9 to i8* 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %12 = bitcast i8* %11 to double* 
  store  double 3.200000e1, double* %12, align 8 
  %13 = bitcast double* %12 to i8* 
  %14 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %10, i32  1, i8*  %13)  
  %15 = bitcast i8* %14 to i8* 
  ret void 
}