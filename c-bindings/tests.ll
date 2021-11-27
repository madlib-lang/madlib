; ModuleID = 'number'


 


declare external ccc  i8* @__applyPAP__(i8*, i32, ...)    


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


declare external ccc  i1* @__eqInteger__(i8*, i8*)    


declare external ccc  i1* @__eqByte__(i8*, i8*)    


declare external ccc  i1* @__eqFloat__(i8*, i8*)    


declare external ccc  i1* @__eqString__(i8*, i8*)    


declare external ccc  i1* @__eqBoolean__(i8*, i8*)    


declare external ccc  i1* @__eqList__(i8*, i8*, i8*)    


define external ccc  i1* @$lambda$lifted$EqList(i8*  %eqDict_0, i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %eqDict_0 to i8* 
  %2 = bitcast i8* %a_0 to i8* 
  %3 = bitcast i8* %b_0 to i8* 
  %4 =  call ccc  i1*  @__eqList__(i8*  %eqDict_0, i8*  %a_0, i8*  %b_0)  
  ret i1* %4
}


define external ccc  i8* @"$Number$Integer$*"(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  i64*  @__multiplyIntegers__(i8*  %a_0, i8*  %b_0)  
  %4 = bitcast i64* %3 to i8* 
  ret i64* %3 
}


define external ccc  i8* @"$Number$Integer$+"(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  i64*  @__addIntegers__(i8*  %a_0, i8*  %b_0)  
  %4 = bitcast i64* %3 to i8* 
  ret i64* %3 
}


define external ccc  i8* @$Number$Integer$-(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  i64*  @__substractIntegers__(i8*  %a_0, i8*  %b_0)  
  %4 = bitcast i64* %3 to i8* 
  ret i64* %3 
}


define external ccc  i8* @$Number$Integer$__coerceNumber__(i8*  %a_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 =  call ccc  i8*  @__numberToInteger__(i8*  %a_0)  
  %3 = bitcast i8* %2 to i8* 
  ret i8* %2 
}


@$Number$Integer =    global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Number$Integer$*" to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Number$Integer$+" to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @$Number$Integer$- to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*)* @$Number$Integer$__coerceNumber__ to i8*), i32 1, i32 1, i8* undef } }


define external ccc  i8* @"$Number$Byte$*"(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  double*  @__multiplyFloats__(i8*  %a_0, i8*  %b_0)  
  %4 = bitcast double* %3 to i8* 
  ret double* %3 
}


define external ccc  i8* @"$Number$Byte$+"(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  double*  @__addFloats__(i8*  %a_0, i8*  %b_0)  
  %4 = bitcast double* %3 to i8* 
  ret double* %3 
}


define external ccc  i8* @$Number$Byte$-(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  double*  @__substractFloats__(i8*  %a_0, i8*  %b_0)  
  %4 = bitcast double* %3 to i8* 
  ret double* %3 
}


define external ccc  i8* @$Number$Byte$__coerceNumber__(i8*  %a_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 =  call ccc  i8*  @__numberToFloat__(i8*  %a_0)  
  %3 = bitcast i8* %2 to i8* 
  ret i8* %2 
}


@$Number$Byte =    global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Number$Byte$*" to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Number$Byte$+" to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @$Number$Byte$- to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*)* @$Number$Byte$__coerceNumber__ to i8*), i32 1, i32 1, i8* undef } }


define external ccc  i8* @"$Number$Float$*"(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  double*  @__multiplyFloats__(i8*  %a_0, i8*  %b_0)  
  %4 = bitcast double* %3 to i8* 
  ret double* %3 
}


define external ccc  i8* @"$Number$Float$+"(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  double*  @__addFloats__(i8*  %a_0, i8*  %b_0)  
  %4 = bitcast double* %3 to i8* 
  ret double* %3 
}


define external ccc  i8* @$Number$Float$-(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  double*  @__substractFloats__(i8*  %a_0, i8*  %b_0)  
  %4 = bitcast double* %3 to i8* 
  ret double* %3 
}


define external ccc  i8* @$Number$Float$__coerceNumber__(i8*  %a_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 =  call ccc  i8*  @__numberToFloat__(i8*  %a_0)  
  %3 = bitcast i8* %2 to i8* 
  ret i8* %2 
}


@$Number$Float =    global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Number$Float$*" to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Number$Float$+" to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @$Number$Float$- to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*)* @$Number$Float$__coerceNumber__ to i8*), i32 1, i32 1, i8* undef } }


define external ccc  i8* @"$Eq$Integer$=="(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  i1*  @__eqInteger__(i8*  %a_0, i8*  %b_0)  
  %4 = bitcast i1* %3 to i8* 
  ret i1* %3 
}


@$Eq$Integer =    global {{i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Eq$Integer$==" to i8*), i32 2, i32 2, i8* undef } }


define external ccc  i8* @"$Eq$Float$=="(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  i1*  @__eqInteger__(i8*  %a_0, i8*  %b_0)  
  %4 = bitcast i1* %3 to i8* 
  ret i1* %3 
}


@$Eq$Float =    global {{i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Eq$Float$==" to i8*), i32 2, i32 2, i8* undef } }


define external ccc  i8* @"$Eq$String$=="(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  i1*  @__eqString__(i8*  %a_0, i8*  %b_0)  
  %4 = bitcast i1* %3 to i8* 
  ret i1* %3 
}


@$Eq$String =    global {{i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Eq$String$==" to i8*), i32 2, i32 2, i8* undef } }


define external ccc  i8* @"$Eq$Boolean$=="(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 =  call ccc  i1*  @__eqBoolean__(i8*  %a_0, i8*  %b_0)  
  %4 = bitcast i1* %3 to i8* 
  ret i1* %3 
}


@$Eq$Boolean =    global {{i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @"$Eq$Boolean$==" to i8*), i32 2, i32 2, i8* undef } }


define external ccc  i8* @"$Eq$List$=="(i8*  %eqDict_0)    {
  %1 = bitcast i8* %eqDict_0 to i8* 
  %2 = bitcast i8* (i8*, i8*, i8*)* @$lambda$lifted$EqList to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*}* 
  %5 = getelementptr  {i8*}, {i8*}* %4, i32 0, i32 0 
  store  i8* %eqDict_0, i8** %5, align 8 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i8*, i32, i32, i8*}* 
  %8 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 0 
  store  i8* %2, i8** %8, align 8 
  %9 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 1 
  store  i32 3, i32* %9, align 8 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 2 
  store  i32 2, i32* %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 3 
  store  i8* %3, i8** %11, align 8 
  ret i8* %6 
}