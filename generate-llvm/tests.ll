; ModuleID = 'main'


 


declare external ccc  i8* @__5dd8d8f67f6caa3e4c41a3cdcb44ab61__prettyLog(i8*, i8*)    


declare external ccc  i8* @__61ebaf0b80ceb01f7afef988602b6f1f__filter(i8*, i8*)    


declare external ccc  i8* @__61ebaf0b80ceb01f7afef988602b6f1f__reject(i8*, i8*)    


declare external ccc  i8* @__61ebaf0b80ceb01f7afef988602b6f1f__reverse(i8*)    


declare external ccc  i8* @__61ebaf0b80ceb01f7afef988602b6f1f__len(i8*)    


@$Functor$List = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$Functor$List$map(i8*, i8*)    


@$Monoid$List = external   global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} 


declare external ccc  i8* @$Monoid$List$mappend(i8*, i8*)    


declare external ccc  i8* @$Monoid$List$mempty()    


@$PrettyShow$Boolean = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$PrettyShow$Boolean$ppShow(i8*)    


@$PrettyShow$List = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$PrettyShow$List$ppShow(i8*, i8*)    


@$PrettyShow$Number = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$PrettyShow$Number$ppShow(i8*)    


@$PrettyShow$String = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$PrettyShow$String$ppShow(i8*)    


@$Semigroup$List = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$Semigroup$List$assoc(i8*, i8*)    


@$Show$Boolean = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$Show$Boolean$show(i8*)    


@$Show$List = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$Show$List$show(i8*)    


@$Show$Number = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$Show$Number$show(i8*)    


@$Show$String = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$Show$String$show(i8*)    


define external ccc  i8* @__20f864705719354610b890bb46cbc029__inc(i8*  %a_0)    {
  %1 = bitcast i8* %a_0 to i8 addrspace(1)** 
  %2 = load  i8 addrspace(1)*, i8 addrspace(1)** %1, align 8 
  %3 =  call ccc  i8*  @GC_malloc(i64  2)  
  %4 = addrspacecast i8* %3 to i8 addrspace(1)* 
  %5 = getelementptr  i8, i8 addrspace(1)* %4, i32 0 
  store  i8 49, i8 addrspace(1)* %5, align 8 
  %6 = getelementptr  i8, i8 addrspace(1)* %4, i32 1 
  store  i8 0, i8 addrspace(1)* %6, align 8 
  %7 =  call ccc  i8 addrspace(1)*  @__strConcat__(i8 addrspace(1)*  %4, i8 addrspace(1)*  %2)  
  %8 = bitcast i8 addrspace(1)* %4 to i8 addrspace(1)** 
  store  i8 addrspace(1)* %7, i8 addrspace(1)** %8, align 8 
  %9 =  call ccc  i8 addrspace(1)*  @__strConcat__(i8 addrspace(1)*  %2, i8 addrspace(1)*  %7)  
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i8 addrspace(1)** getelementptr inbounds (i8 addrspace(1)*, i8 addrspace(1)** inttoptr (i32 0 to i8 addrspace(1)**), i32 1) to i64))  
  %11 = bitcast i8* %10 to i8 addrspace(1)** 
  store  i8 addrspace(1)* %9, i8 addrspace(1)** %11, align 8 
  %12 = bitcast i8 addrspace(1)** %11 to i8* 
  ret i8* %12 
}


declare external ccc  i8* @__applyPAP__(i8*, i32, ...)    


declare external ccc  {i32, i8*}* @__buildRecord__(i32, i8*, ...)    


declare external ccc  i8* @__selectField__(i8 addrspace(1)*, {i32, i8*}*)    


declare external ccc  i1 @__streq__(i8 addrspace(1)*, i8 addrspace(1)*)    


declare external ccc  i8 addrspace(1)* @__strConcat__(i8 addrspace(1)*, i8 addrspace(1)*)    


declare external ccc  i1 @MadList_hasMinLength(double, {i8*, i8*}*)    


declare external ccc  i1 @MadList_hasLength(double, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_singleton(i8*)    


declare external ccc  {i8*, i8*}* @__MadList_push__(i8*, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_concat({i8*, i8*}*, {i8*, i8*}*)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  void @__0ca498e5e1cdf5d72e5690ca02bcce97__moduleFunction()    


declare external ccc  void @__5dd8d8f67f6caa3e4c41a3cdcb44ab61__moduleFunction()    


declare external ccc  void @__61ebaf0b80ceb01f7afef988602b6f1f__moduleFunction()    


declare external ccc  void @__8d7c3fd7b9b6d6041bebb255cb798333__moduleFunction()    


declare external ccc  void @__b4db05d793a8756e66d267415704250c__moduleFunction()    


declare external ccc  void @__c700acae529ff947acae37ffd524bdc6__moduleFunction()    


declare external ccc  void @__c74b94e1f65bda9e07e07469839aae9a__moduleFunction()    


declare external ccc  void @__cebca4c98d7d0c8157e1ed2c6179f816__moduleFunction()    


declare external ccc  void @__cf4f67c6dc8d68bc46cd8acbde6afb1a__moduleFunction()    


declare external ccc  void @__e3e8c77e8e733398f092072b275bef7e__moduleFunction()    


declare external ccc  void @__e7379b924ed07f9bcd24bc2d5e58171f__moduleFunction()    


declare external ccc  void @__f5f170f8b96f1ba835b8d043e370378e__moduleFunction()    


define external ccc  void @main()    {
entry_0:
   call ccc  void  @__0ca498e5e1cdf5d72e5690ca02bcce97__moduleFunction()  
   call ccc  void  @__5dd8d8f67f6caa3e4c41a3cdcb44ab61__moduleFunction()  
   call ccc  void  @__61ebaf0b80ceb01f7afef988602b6f1f__moduleFunction()  
   call ccc  void  @__8d7c3fd7b9b6d6041bebb255cb798333__moduleFunction()  
   call ccc  void  @__b4db05d793a8756e66d267415704250c__moduleFunction()  
   call ccc  void  @__c700acae529ff947acae37ffd524bdc6__moduleFunction()  
   call ccc  void  @__c74b94e1f65bda9e07e07469839aae9a__moduleFunction()  
   call ccc  void  @__cebca4c98d7d0c8157e1ed2c6179f816__moduleFunction()  
   call ccc  void  @__cf4f67c6dc8d68bc46cd8acbde6afb1a__moduleFunction()  
   call ccc  void  @__e3e8c77e8e733398f092072b275bef7e__moduleFunction()  
   call ccc  void  @__e7379b924ed07f9bcd24bc2d5e58171f__moduleFunction()  
   call ccc  void  @__f5f170f8b96f1ba835b8d043e370378e__moduleFunction()  
  %0 = bitcast i8* (i8*, i8*)* @__5dd8d8f67f6caa3e4c41a3cdcb44ab61__prettyLog to i8* 
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i8*, i32, i32, i8*}* 
  %3 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 0 
  store  i8* %0, i8** %3, align 8 
  %4 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 1 
  store  i32 2, i32* %4, align 8 
  %5 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 2 
  store  i32 2, i32* %5, align 8 
  %6 = bitcast {i8*, i32, i32, i8*}* %2 to i8* 
  %7 = bitcast {{i8*, i32, i32, i8*}}* @$Show$String to i8* 
  %8 = bitcast {{i8*, i32, i32, i8*}}* @$PrettyShow$String to i8* 
  %9 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %6, i32  2, i8*  %7, i8*  %8)  
  %10 = bitcast i8* %9 to {i8*, i32, i32, i8*}* 
  %11 = bitcast {i8*, i32, i32, i8*}* %10 to i8* 
  %12 =  call ccc  i8*  @GC_malloc(i64  2)  
  %13 = addrspacecast i8* %12 to i8 addrspace(1)* 
  %14 = getelementptr  i8, i8 addrspace(1)* %13, i32 0 
  store  i8 52, i8* %14, align 8 
  %15 = getelementptr  i8, i8 addrspace(1)* %13, i32 1 
  store  i8 0, i8* %15, align 8 
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i8 addrspace(1)** getelementptr inbounds (i8 addrspace(1)*, i8 addrspace(1)** inttoptr (i32 0 to i8 addrspace(1)**), i32 1) to i64))  
  %17 = bitcast i8* %16 to i8 addrspace(1)** 
  store  i8 addrspace(1)* %13, i8 addrspace(1)** %17, align 8 
  %18 = bitcast i8 addrspace(1)** %17 to i8* 
  %19 =  call ccc  i8*  @__20f864705719354610b890bb46cbc029__inc(i8*  %18)  
  %20 = bitcast i8* %19 to i8 addrspace(1)** 
  %21 = load  i8 addrspace(1)*, i8 addrspace(1)** %20, align 8 
  %22 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i8 addrspace(1)** getelementptr inbounds (i8 addrspace(1)*, i8 addrspace(1)** inttoptr (i32 0 to i8 addrspace(1)**), i32 1) to i64))  
  %23 = bitcast i8* %22 to i8 addrspace(1)** 
  store  i8 addrspace(1)* %21, i8 addrspace(1)** %23, align 8 
  %24 = bitcast i8 addrspace(1)** %23 to i8* 
  %25 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %11, i32  1, i8*  %24)  
  %26 = bitcast i8* %25 to i8* 
  ret void 
}