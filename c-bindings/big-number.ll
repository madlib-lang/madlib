; ModuleID = 'main'


 


; @$Number$Byte = external   global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} 


; declare external ccc  i8* @"$Number$Byte$*"(i8*, i8*)    


; declare external ccc  i8* @"$Number$Byte$+"(i8*, i8*)    


; declare external ccc  i8* @$Number$Byte$-(i8*, i8*)    


; declare external ccc  i8* @$Number$Byte$__coerceNumber__(i8*)    


; @$Number$Float = external   global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} 


; declare external ccc  i8* @"$Number$Float$*"(i8*, i8*)    


; declare external ccc  i8* @"$Number$Float$+"(i8*, i8*)    


; declare external ccc  i8* @$Number$Float$-(i8*, i8*)    


; declare external ccc  i8* @$Number$Float$__coerceNumber__(i8*)    


; @$Number$Integer = external   global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} 


; declare external ccc  i8* @"$Number$Integer$*"(i8*, i8*)    


; declare external ccc  i8* @"$Number$Integer$+"(i8*, i8*)    


; declare external ccc  i8* @$Number$Integer$-(i8*, i8*)    


; declare external ccc  i8* @$Number$Integer$__coerceNumber__(i8*)    


declare external ccc  {i8*, i8*}* @MadList_push(i8*, {i8*, i8*}*)    


define external ccc  i8* @__edc209afd0927a85d7d119088a377001__push(i8* , i8* )    {
  %3 = bitcast i8* %0 to i8* 
  %4 = bitcast i8* %1 to {i8*, i8*}** 
  %5 = load  {i8*, i8*}*, {i8*, i8*}** %4, align 8 
  %6 =  call ccc  {i8*, i8*}*  @MadList_push(i8*  %3, {i8*, i8*}*  %5)  
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}** getelementptr inbounds ({i8*, i8*}*, {i8*, i8*}** inttoptr (i32 0 to {i8*, i8*}**), i32 1) to i64))  
  %8 = bitcast i8* %7 to {i8*, i8*}** 
  store  {i8*, i8*}* %6, {i8*, i8*}** %8, align 8 
  %9 = bitcast {i8*, i8*}** %8 to i8* 
  ret i8* %9 
}


define external ccc  i8* @__edc209afd0927a85d7d119088a377001__ch(i8*  %count_0, i8*  %acc_0)    {
; <label>:0:
  %1 = bitcast i8* %count_0 to i64* 
  %2 = load  i64, i64* %1, align 8 
  %3 = bitcast i8* %acc_0 to {i8*, i8*}** 
  %4 = load  {i8*, i8*}*, {i8*, i8*}** %3, align 8 
  %5 = icmp eq i64 %2, 0 
  %6 = icmp eq i1 %5, 1 
  br i1 %6, label %truthyBlock_0, label %falsyBlock_0 
truthyBlock_0:
  br label %condBlock_0 
falsyBlock_0:
  %7 = sub   i64 %2, 1 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %9 = bitcast i8* %8 to i64* 
  store  i64 9, i64* %9, align 8 
  %10 = bitcast i64* %9 to i8* 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}** getelementptr inbounds ({i8*, i8*}*, {i8*, i8*}** inttoptr (i32 0 to {i8*, i8*}**), i32 1) to i64))  
  %12 = bitcast i8* %11 to {i8*, i8*}** 
  store  {i8*, i8*}* %4, {i8*, i8*}** %12, align 8 
  %13 = bitcast {i8*, i8*}** %12 to i8* 
  %14 =  call ccc  i8*  @__edc209afd0927a85d7d119088a377001__push(i8*  %10, i8*  %13)  
  %15 = bitcast i8* %14 to {i8*, i8*}** 
  %16 = load  {i8*, i8*}*, {i8*, i8*}** %15, align 8 
  %17 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %18 = bitcast i8* %17 to i64* 
  store  i64 %7, i64* %18, align 8 
  %19 = bitcast i64* %18 to i8* 
  %20 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}** getelementptr inbounds ({i8*, i8*}*, {i8*, i8*}** inttoptr (i32 0 to {i8*, i8*}**), i32 1) to i64))  
  %21 = bitcast i8* %20 to {i8*, i8*}** 
  store  {i8*, i8*}* %16, {i8*, i8*}** %21, align 8 
  %22 = bitcast {i8*, i8*}** %21 to i8* 
  %23 =  call ccc  i8*  @__edc209afd0927a85d7d119088a377001__ch(i8*  %19, i8*  %22)  
  %24 = bitcast i8* %23 to {i8*, i8*}** 
  %25 = load  {i8*, i8*}*, {i8*, i8*}** %24, align 8 
  br label %condBlock_0 
condBlock_0:
  %26 = phi {i8*, i8*}* [%4, %truthyBlock_0], [%25, %falsyBlock_0] 
  %27 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}** getelementptr inbounds ({i8*, i8*}*, {i8*, i8*}** inttoptr (i32 0 to {i8*, i8*}**), i32 1) to i64))  
  %28 = bitcast i8* %27 to {i8*, i8*}** 
  store  {i8*, i8*}* %26, {i8*, i8*}** %28, align 8 
  %29 = bitcast {i8*, i8*}** %28 to i8* 
  ret i8* %29 
}


define external ccc  i8* @__edc209afd0927a85d7d119088a377001__createBigList(i8*  %count_0)    {
  %1 = bitcast i8* %count_0 to i64* 
  %2 = load  i64, i64* %1, align 8 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*, i8*}* 
  store  {i8*, i8*} { i8* zeroinitializer, i8* zeroinitializer }, {i8*, i8*}* %4, align 8 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %6 = bitcast i8* %5 to i64* 
  store  i64 %2, i64* %6, align 8 
  %7 = bitcast i64* %6 to i8* 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}** getelementptr inbounds ({i8*, i8*}*, {i8*, i8*}** inttoptr (i32 0 to {i8*, i8*}**), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8*, i8*}** 
  store  {i8*, i8*}* %4, {i8*, i8*}** %9, align 8 
  %10 = bitcast {i8*, i8*}** %9 to i8* 
  %11 =  call ccc  i8*  @__edc209afd0927a85d7d119088a377001__ch(i8*  %7, i8*  %10)  
  %12 = bitcast i8* %11 to {i8*, i8*}** 
  %13 = load  {i8*, i8*}*, {i8*, i8*}** %12, align 8 
  %14 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}** getelementptr inbounds ({i8*, i8*}*, {i8*, i8*}** inttoptr (i32 0 to {i8*, i8*}**), i32 1) to i64))  
  %15 = bitcast i8* %14 to {i8*, i8*}** 
  store  {i8*, i8*}* %13, {i8*, i8*}** %15, align 8 
  %16 = bitcast {i8*, i8*}** %15 to i8* 
  ret i8* %16 
}


declare external ccc  i8* @__applyPAP__(i8*, i32, ...)    


declare external ccc  {i32, i8*}* @__buildRecord__(i32, i8*, ...)    


declare external ccc  i8* @__selectField__(i8*, {i32, i8*}*)    


declare external ccc  i1 @__streq__(i8*, i8*)    


declare external ccc  i8* @__strConcat__(i8*, i8*)    


declare external ccc  i1 @MadList_hasMinLength(double, {i8*, i8*}*)    


declare external ccc  i1 @MadList_hasLength(double, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_singleton(i8*)    


declare external ccc  {i8*, i8*}* @__MadList_push__(i8*, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_concat({i8*, i8*}*, {i8*, i8*}*)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  void @__initEventLoop__()    


declare external ccc  void @__startEventLoop__()    


@__edc209afd0927a85d7d119088a377001__big =    global {i8*, i8*}* undef


define external ccc  void @main()    {
entry_0:
   call ccc  void  @__initEventLoop__()  
  %0 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %1 = bitcast i8* %0 to {i8*, i8*}* 
  store  {i8*, i8*} { i8* zeroinitializer, i8* zeroinitializer }, {i8*, i8*}* %1, align 8 
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %3 = bitcast i8* %2 to i64* 
  store  i64 1000000, i64* %3, align 8 
  %4 = bitcast i64* %3 to i8* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}** getelementptr inbounds ({i8*, i8*}*, {i8*, i8*}** inttoptr (i32 0 to {i8*, i8*}**), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8*, i8*}** 
  store  {i8*, i8*}* %1, {i8*, i8*}** %6, align 8 
  %7 = bitcast {i8*, i8*}** %6 to i8* 
  %8 =  tail call fastcc  i8*  @__edc209afd0927a85d7d119088a377001__ch(i8*  %4, i8*  %7)  
  %9 = bitcast i8* %8 to {i8*, i8*}** 
  %10 = load  {i8*, i8*}*, {i8*, i8*}** %9, align 8 
  store  {i8*, i8*}* %10, {i8*, i8*}** @__edc209afd0927a85d7d119088a377001__big, align 8 
   call ccc  void  @__startEventLoop__()  
  ret void 
}

; ModuleID = 'number'


 


declare external ccc  i64* @__addIntegers__(i8*, i8*)    


declare external ccc  i64* @__substractIntegers__(i8*, i8*)    


declare external ccc  i64* @__multiplyIntegers__(i8*, i8*)    


declare external ccc  i8* @__numberToInteger__(i8*)    


declare external ccc  i8* @__addBytes__(i8*, i8*)    


declare external ccc  i8* @__substractBytes__(i8*, i8*)    


declare external ccc  i8* @__multiplyBytes__(i8*, i8*)    


declare external ccc  i8* @__numberToByte__(i8*)    


declare external ccc  double* @__addFloats__(i8*, i8*)    


declare external ccc  double* @__substractFloats__(i8*, i8*)    


declare external ccc  double* @__multiplyFloats__(i8*, i8*)    


declare external ccc  i8* @__numberToFloat__(i8*)    


define external ccc  i8* @"$Number$Integer$*"(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  i64*  @__multiplyIntegers__(i8*  %1, i8*  %2)  
  %4 = bitcast i64* %3 to i8* 
  ret i8* %4 
}


define external ccc  i8* @"$Number$Integer$+"(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  i64*  @__addIntegers__(i8*  %1, i8*  %2)  
  %4 = bitcast i64* %3 to i8* 
  ret i8* %4 
}


define external ccc  i8* @$Number$Integer$-(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  i64*  @__substractIntegers__(i8*  %1, i8*  %2)  
  %4 = bitcast i64* %3 to i8* 
  ret i8* %4 
}


define external ccc  i8* @$Number$Integer$__coerceNumber__(i8*  %a_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 =  call ccc  i8*  @__numberToInteger__(i8*  %1)  
  %3 = bitcast i8* %2 to i8* 
  ret i8* %3 
}


@$Number$Integer =    global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Number$Integer$*" to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Number$Integer$+" to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @$Number$Integer$- to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*)* @$Number$Integer$__coerceNumber__ to i8*), i32 1, i32 1, i8* undef } }


define external ccc  i8* @"$Number$Byte$*"(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  double*  @__multiplyFloats__(i8*  %1, i8*  %2)  
  %4 = bitcast double* %3 to i8* 
  ret i8* %4 
}


define external ccc  i8* @"$Number$Byte$+"(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  double*  @__addFloats__(i8*  %1, i8*  %2)  
  %4 = bitcast double* %3 to i8* 
  ret i8* %4 
}


define external ccc  i8* @$Number$Byte$-(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  double*  @__substractFloats__(i8*  %1, i8*  %2)  
  %4 = bitcast double* %3 to i8* 
  ret i8* %4 
}


define external ccc  i8* @$Number$Byte$__coerceNumber__(i8*  %a_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 =  call ccc  i8*  @__numberToFloat__(i8*  %1)  
  %3 = bitcast i8* %2 to i8* 
  ret i8* %3 
}


@$Number$Byte =    global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Number$Byte$*" to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Number$Byte$+" to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @$Number$Byte$- to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*)* @$Number$Byte$__coerceNumber__ to i8*), i32 1, i32 1, i8* undef } }


define external ccc  i8* @"$Number$Float$*"(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  double*  @__multiplyFloats__(i8*  %1, i8*  %2)  
  %4 = bitcast double* %3 to i8* 
  ret i8* %4 
}


define external ccc  i8* @"$Number$Float$+"(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  double*  @__addFloats__(i8*  %1, i8*  %2)  
  %4 = bitcast double* %3 to i8* 
  ret i8* %4 
}


define external ccc  i8* @$Number$Float$-(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  double*  @__substractFloats__(i8*  %1, i8*  %2)  
  %4 = bitcast double* %3 to i8* 
  ret i8* %4 
}


define external ccc  i8* @$Number$Float$__coerceNumber__(i8*  %a_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 =  call ccc  i8*  @__numberToFloat__(i8*  %1)  
  %3 = bitcast i8* %2 to i8* 
  ret i8* %3 
}


@$Number$Float =    global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Number$Float$*" to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Number$Float$+" to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @$Number$Float$- to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*)* @$Number$Float$__coerceNumber__ to i8*), i32 1, i32 1, i8* undef } }