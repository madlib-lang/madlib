; ModuleID = 'cebca4c98d7d0c8157e1ed2c6179f816'


 


@$PrettyShow$Boolean = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$PrettyShow$Boolean$ppShow(i8*)    


@$PrettyShow$Number = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$PrettyShow$Number$ppShow(i8*)    


@$PrettyShow$String = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$PrettyShow$String$ppShow(i8*)    


@$Show$Boolean = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$Show$Boolean$show(i8*)    


@$Show$Number = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$Show$Number$show(i8*)    


@$Show$String = external   global {{i8*, i32, i32, i8*}} 


declare external ccc  i8* @$Show$String$show(i8*)    


define external ccc  i8* @__cebca4c98d7d0c8157e1ed2c6179f816__always(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  ret i8* %1 
}


define external ccc  i8* @__cebca4c98d7d0c8157e1ed2c6179f816__complement(i8*  %fn_0, i8*  %x_0)    {
  %1 = bitcast i8* %fn_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %x_0 to i8* 
  %3 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %4 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %3, i32  1, i8*  %2)  
  %5 = bitcast i8* %4 to i1* 
  %6 = load  i1, i1* %5, align 8 
  %7 = fadd i1 %6, 1 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %9 = bitcast i8* %8 to i1* 
  store  i1 %7, i1* %9, align 8 
  %10 = bitcast i1* %9 to i8* 
  ret i8* %10 
}


define external ccc  i8* @__cebca4c98d7d0c8157e1ed2c6179f816__identity(i8*  %a_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  ret i8* %1 
}


define external ccc  i8* @__cebca4c98d7d0c8157e1ed2c6179f816__ifElse(i8*  %predicate_0, i8*  %truthy_0, i8*  %falsy_0, i8*  %value_0)    {
; <label>:0:
  %1 = bitcast i8* %predicate_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %truthy_0 to {i8*, i32, i32, i8*}* 
  %3 = bitcast i8* %falsy_0 to {i8*, i32, i32, i8*}* 
  %4 = bitcast i8* %value_0 to i8* 
  %5 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %6 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %5, i32  1, i8*  %4)  
  %7 = bitcast i8* %6 to i1* 
  %8 = load  i1, i1* %7, align 8 
  %9 = icmp eq i1 %8, 1 
  br i1 %9, label %truthyBlock_0, label %falsyBlock_0 
truthyBlock_0:
  %10 = bitcast {i8*, i32, i32, i8*}* %2 to i8* 
  %11 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %10, i32  1, i8*  %4)  
  %12 = bitcast i8* %11 to i8* 
  br label %condBlock_0 
falsyBlock_0:
  %13 = bitcast {i8*, i32, i32, i8*}* %3 to i8* 
  %14 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %13, i32  1, i8*  %4)  
  %15 = bitcast i8* %14 to i8* 
  br label %condBlock_0 
condBlock_0:
  %16 = phi i8* [%12, %truthyBlock_0], [%15, %falsyBlock_0] 
  %17 = bitcast i8* %16 to i8* 
  ret i8* %17 
}


define external ccc  i8* @__cebca4c98d7d0c8157e1ed2c6179f816__when(i8*  %predicate_0, i8*  %truthy_0, i8*  %value_0)    {
  %1 = bitcast i8* %predicate_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %truthy_0 to {i8*, i32, i32, i8*}* 
  %3 = bitcast i8* %value_0 to i8* 
  %4 = bitcast i8* (i8*, i8*)* @__cebca4c98d7d0c8157e1ed2c6179f816__always to i8* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8*}* 
  %7 = getelementptr  {i8*}, {i8*}* %6, i32 0, i32 0 
  store  i8* %3, i8** %7, align 8 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8*, i32, i32, i8*}* 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 0 
  store  i8* %4, i8** %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 1 
  store  i32 2, i32* %11, align 8 
  %12 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 2 
  store  i32 1, i32* %12, align 8 
  %13 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 3 
  store  i8* %5, i8** %13, align 8 
  %14 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %15 = bitcast {i8*, i32, i32, i8*}* %2 to i8* 
  %16 = bitcast {i8*, i32, i32, i8*}* %9 to i8* 
  %17 =  call ccc  i8*  @__cebca4c98d7d0c8157e1ed2c6179f816__ifElse(i8*  %14, i8*  %15, i8*  %16, i8*  %3)  
  %18 = bitcast i8* %17 to i8* 
  ret i8* %18 
}


define external ccc  i8* @__cebca4c98d7d0c8157e1ed2c6179f816__not(i8*  %b_0)    {
  %1 = bitcast i8* %b_0 to i1* 
  %2 = load  i1, i1* %1, align 8 
  %3 = fadd i1 %2, 1 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %5 = bitcast i8* %4 to i1* 
  store  i1 %3, i1* %5, align 8 
  %6 = bitcast i1* %5 to i8* 
  ret i8* %6 
}


define external ccc  i8* @__cebca4c98d7d0c8157e1ed2c6179f816__flip(i8*  %f_0, i8*  %b_0, i8*  %a_0)    {
  %1 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 = bitcast i8* %a_0 to i8* 
  %4 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %5 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %4, i32  2, i8*  %3, i8*  %2)  
  %6 = bitcast i8* %5 to i8* 
  ret i8* %6 
}


declare external ccc  i8* @__applyPAP__(i8*, i32, ...)    


declare external ccc  i8* @__buildRecord__(i32, i8*, ...)    


declare external ccc  i8* @__selectField__(i8*, i8*)    


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


define external ccc  void @__cebca4c98d7d0c8157e1ed2c6179f816__moduleFunction()    {
entry_0:
  ret void 
}