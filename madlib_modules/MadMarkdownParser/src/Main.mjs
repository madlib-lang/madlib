// file: /home/runner/work/madlib/madlib/.github/workflows/madlib_modules/MadMarkdownParser/src/Main.mad
import {} from "./../../../__internals__.mjs"
import { dropWhile, filter } from "./../../../.prelude/List.mjs";
import { always, equals, identity } from "./../../../.prelude/Function.mjs";
import { Just, Nothing } from "./../../../.prelude/Maybe.mjs";
import { mapLeft } from "./../../../.prelude/Either.mjs";
import { mapL } from "./../../../.prelude/Functor.mjs";
import { apL } from "./../../../.prelude/Applicative.mjs";
import P from "./../../../.prelude/Parse.mjs";
import String from "./../../../.prelude/String.mjs";

export let Text = (a => ({ __constructor: "Text", __args: [ a ] }));
export let Bold = (a => ({ __constructor: "Bold", __args: [ a ] }));
export let Italic = (a => ({ __constructor: "Italic", __args: [ a ] }));
export let InlineCode = (a => ({ __constructor: "InlineCode", __args: [ a ] }));
export let Link = (a => b => ({ __constructor: "Link", __args: [ a, b ] }));
export let Image = (a => b => ({ __constructor: "Image", __args: [ a, b ] }));
export let LineReturn = ({ __constructor: "LineReturn", __args: [  ] });
export let H1 = (a => ({ __constructor: "H1", __args: [ a ] }));
export let H2 = (a => ({ __constructor: "H2", __args: [ a ] }));
export let H3 = (a => ({ __constructor: "H3", __args: [ a ] }));
export let H4 = (a => ({ __constructor: "H4", __args: [ a ] }));
export let H5 = (a => ({ __constructor: "H5", __args: [ a ] }));
export let H6 = (a => ({ __constructor: "H6", __args: [ a ] }));
export let Paragraph = (a => ({ __constructor: "Paragraph", __args: [ a ] }));
export let Blockquote = (a => ({ __constructor: "Blockquote", __args: [ a ] }));
export let Code = (a => b => ({ __constructor: "Code", __args: [ a, b ] }));
export let UnorderedList = (a => ({ __constructor: "UnorderedList", __args: [ a ] }));
Inspect['ContentPart_d248b95ce983f5117125258d627bcc0b'] = {};
Inspect['ContentPart_d248b95ce983f5117125258d627bcc0b']['inspect'] = () => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "Text" && true) {
    let a0 = __x__.__args[0];
    return `Text(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
  }
  else if (__x__.__constructor === "Bold" && true) {
    let a0 = __x__.__args[0];
    return `Bold(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
  }
  else if (__x__.__constructor === "Italic" && true) {
    let a0 = __x__.__args[0];
    return `Italic(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
  }
  else if (__x__.__constructor === "InlineCode" && true) {
    let a0 = __x__.__args[0];
    return `InlineCode(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `)`;
  }
  else if (__x__.__constructor === "Link" && true && true) {
    let a0 = __x__.__args[0];
    let a1 = __x__.__args[1];
    return `Link(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a1) + `)`;
  }
  else if (__x__.__constructor === "Image" && true && true) {
    let a0 = __x__.__args[0];
    let a1 = __x__.__args[1];
    return `Image(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a1) + `)`;
  }
  else if (__x__.__constructor === "LineReturn") {
    return `LineReturn`;
  }
  else if (true) {
    return `Unknown`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__$a__));
Inspect['Block_d248b95ce983f5117125258d627bcc0b'] = {};
Inspect['Block_d248b95ce983f5117125258d627bcc0b']['inspect'] = () => (__$a__ => ((__x__) => {
  if (__x__.__constructor === "H1" && true) {
    let a0 = __x__.__args[0];
    return `H1(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
  }
  else if (__x__.__constructor === "H2" && true) {
    let a0 = __x__.__args[0];
    return `H2(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
  }
  else if (__x__.__constructor === "H3" && true) {
    let a0 = __x__.__args[0];
    return `H3(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
  }
  else if (__x__.__constructor === "H4" && true) {
    let a0 = __x__.__args[0];
    return `H4(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
  }
  else if (__x__.__constructor === "H5" && true) {
    let a0 = __x__.__args[0];
    return `H5(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
  }
  else if (__x__.__constructor === "H6" && true) {
    let a0 = __x__.__args[0];
    return `H6(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
  }
  else if (__x__.__constructor === "Paragraph" && true) {
    let a0 = __x__.__args[0];
    return `Paragraph(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
  }
  else if (__x__.__constructor === "Blockquote" && true) {
    let a0 = __x__.__args[0];
    return `Blockquote(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b)(a0) + `)`;
  }
  else if (__x__.__constructor === "Code" && true && true) {
    let a0 = __x__.__args[0];
    let a1 = __x__.__args[1];
    return `Code(` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a0) + `, ` + Inspect.String_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(a1) + `)`;
  }
  else if (__x__.__constructor === "UnorderedList" && true) {
    let a0 = __x__.__args[0];
    return `UnorderedList(` + Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.inspect()(__apMtdDicts__(Inspect.List_5b7ebeeaa5acfe1eeea5a9e9845b152d, [Inspect.ContentPart_d248b95ce983f5117125258d627bcc0b]))(a0) + `)`;
  }
  else if (true) {
    return `Unknown`;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__$a__));
let between = (start => mid => end => (_P_ => (__$PH2__ => apL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Applicative.Parser_23f791debc71d215453d5deb93f41ac8)(__$PH2__)(end))((__$PH1__ => Applicative.Parser_23f791debc71d215453d5deb93f41ac8.ap()(__$PH1__)(mid))(mapL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(identity)(_P_))))(start));
let contentCharacter = P.choice(({ v: P.letter, n: { v: P.digit, n: { v: P.char(__String.fromCharCode(33)), n: { v: P.char(__String.fromCharCode(63)), n: { v: P.char(__String.fromCharCode(32)), n: null } } } } }));
let linkCharacter = P.choice(({ v: P.letter, n: { v: P.digit, n: { v: P.char(__String.fromCharCode(33)), n: { v: P.char(__String.fromCharCode(35)), n: { v: P.char(__String.fromCharCode(36)), n: { v: P.char(__String.fromCharCode(37)), n: { v: P.char(__String.fromCharCode(38)), n: { v: P.char(__String.fromCharCode(39)), n: { v: P.char(__String.fromCharCode(42)), n: { v: P.char(__String.fromCharCode(43)), n: { v: P.char(__String.fromCharCode(44)), n: { v: P.char(__String.fromCharCode(45)), n: { v: P.char(__String.fromCharCode(46)), n: { v: P.char(__String.fromCharCode(47)), n: { v: P.char(__String.fromCharCode(58)), n: { v: P.char(__String.fromCharCode(59)), n: { v: P.char(__String.fromCharCode(61)), n: { v: P.char(__String.fromCharCode(63)), n: { v: P.char(__String.fromCharCode(64)), n: { v: P.char(__String.fromCharCode(95)), n: { v: P.char(__String.fromCharCode(126)), n: null } } } } } } } } } } } } } } } } } } } } }));
let bold = (_P_ => (__$PH4__ => apL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Applicative.Parser_23f791debc71d215453d5deb93f41ac8)(__$PH4__)(P.string(`**`)))((__$PH3__ => Applicative.Parser_23f791debc71d215453d5deb93f41ac8.ap()(__$PH3__)((_P_ => Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(String.fromList)((a => P.someTill(a)(P.lookAhead(P.string(`**`))))(_P_)))(P.notChar(__String.fromCharCode(10)))))(mapL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Bold)(_P_))))(P.string(`**`));
let italic = (_P_ => (__$PH7__ => apL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Applicative.Parser_23f791debc71d215453d5deb93f41ac8)(__$PH7__)(P.char(__String.fromCharCode(42))))((__$PH6__ => Applicative.Parser_23f791debc71d215453d5deb93f41ac8.ap()(__$PH6__)((_P_ => Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(String.fromList)(P.many(_P_)))(P.notOneOf(({ v: __String.fromCharCode(42), n: { v: __String.fromCharCode(10), n: null } })))))((__$PH5__ => Applicative.Parser_23f791debc71d215453d5deb93f41ac8.ap()(__$PH5__)(P.notChar(__String.fromCharCode(32))))(mapL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)((a => b => Italic(String.pushChar(a)(b))))(_P_)))))(P.char(__String.fromCharCode(42)));
let inlineCode = (_P_ => (__$PH9__ => apL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Applicative.Parser_23f791debc71d215453d5deb93f41ac8)(__$PH9__)(P.char(__String.fromCharCode(96))))((__$PH8__ => Applicative.Parser_23f791debc71d215453d5deb93f41ac8.ap()(__$PH8__)((_P_ => Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(String.fromList)(P.many(_P_)))(P.notOneOf(({ v: __String.fromCharCode(96), n: { v: __String.fromCharCode(10), n: null } })))))(mapL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(InlineCode)(_P_))))(P.char(__String.fromCharCode(96)));
let link = (_P_ => (__$PH10__ => Applicative.Parser_23f791debc71d215453d5deb93f41ac8.ap()(__$PH10__)(between(P.char(__String.fromCharCode(40)))((_P_ => Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(String.fromList)(P.many(_P_)))(linkCharacter))(P.char(__String.fromCharCode(41)))))(Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(Link)(_P_)))(between(P.char(__String.fromCharCode(91)))((_P_ => Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(String.fromList)(P.many(_P_)))(P.notOneOf(({ v: __String.fromCharCode(93), n: { v: __String.fromCharCode(10), n: null } }))))(P.char(__String.fromCharCode(93))));
let image = (_P_ => (__$PH12__ => Applicative.Parser_23f791debc71d215453d5deb93f41ac8.ap()(__$PH12__)(between(P.char(__String.fromCharCode(40)))((_P_ => Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(String.fromList)(P.many(_P_)))(linkCharacter))(P.char(__String.fromCharCode(41)))))((__$PH11__ => Applicative.Parser_23f791debc71d215453d5deb93f41ac8.ap()(__$PH11__)(between(P.char(__String.fromCharCode(91)))((_P_ => Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(String.fromList)(P.many(_P_)))(P.notOneOf(({ v: __String.fromCharCode(93), n: { v: __String.fromCharCode(10), n: null } }))))(P.char(__String.fromCharCode(93)))))(mapL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Image)(_P_))))(P.char(__String.fromCharCode(33)));
let textTerminals = P.choice(({ v: Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(always(``))(bold), n: { v: Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(always(``))(italic), n: { v: Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(always(``))(inlineCode), n: { v: Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(always(``))(image), n: { v: Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(always(``))(link), n: { v: Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(always(``))(P.eof), n: { v: P.string(`\n`), n: null } } } } } } }));
let text = (_P_ => Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()((_P_ => Text(String.fromList(_P_))))((__$PH13__ => P.someTill(__$PH13__)(P.lookAhead(textTerminals)))(_P_)))(P.notChar(__String.fromCharCode(10)));
let lineReturn = Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(always(LineReturn))(P.char(__String.fromCharCode(10)));
let content = (_P_ => P.many(P.choice(_P_)))(({ v: bold, n: { v: italic, n: { v: inlineCode, n: { v: image, n: { v: link, n: { v: text, n: null } } } } } }));
let lineReturnExceptBefore = (before => (_P_ => Monad.Parser_23f791debc71d215453d5deb93f41ac8.chain()((__x__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    return Alternative.Parser_23f791debc71d215453d5deb93f41ac8.aempty();
  }
  else if (__x__.__constructor === "Nothing") {
    return lineReturn;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__)))(P.lookAhead((__$PH14__ => Applicative.Parser_23f791debc71d215453d5deb93f41ac8.ap()(__$PH14__)(Alternative.Parser_23f791debc71d215453d5deb93f41ac8.alt()(Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(always(Just(({ __constructor: "Unit", __args: [] }))))(before))(Applicative.Parser_23f791debc71d215453d5deb93f41ac8.pure()(Nothing))))(mapL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(identity)(_P_)))))(lineReturn));
let contentWithLineReturn = (delimiter => (_P_ => Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(dropWhile(equals(LineReturn)))(P.some(P.choice(_P_))))(({ v: bold, n: { v: italic, n: { v: inlineCode, n: { v: image, n: { v: link, n: { v: text, n: { v: lineReturnExceptBefore(delimiter), n: null } } } } } } })));
let heading = (constructor => _P_ => (__$PH16__ => apL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Applicative.Parser_23f791debc71d215453d5deb93f41ac8)(__$PH16__)(singleReturnTerminal))((__$PH15__ => Applicative.Parser_23f791debc71d215453d5deb93f41ac8.ap()(__$PH15__)(content))(mapL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(constructor)(P.symbol(_P_)))));
let singleReturnTerminal = Alternative.Parser_23f791debc71d215453d5deb93f41ac8.alt()(P.string(`\n`))(Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(always(``))(P.eof));
let doubleReturnTerminal = P.choice(({ v: P.string(`\n\n`), n: { v: Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(always(``))(P.eof), n: { v: (_P_ => (__$PH17__ => Applicative.Parser_23f791debc71d215453d5deb93f41ac8.ap()(__$PH17__)(P.eof))(Applicative.Parser_23f791debc71d215453d5deb93f41ac8.ap()(Applicative.Parser_23f791debc71d215453d5deb93f41ac8.pure()((_ => _ => ``)))(_P_)))(P.char(__String.fromCharCode(10))), n: null } } }));
let code = (_P_ => (__$PH21__ => apL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Applicative.Parser_23f791debc71d215453d5deb93f41ac8)(__$PH21__)(P.choice(({ v: Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()((_ => ``))(apL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Applicative.Parser_23f791debc71d215453d5deb93f41ac8)(P.string(`\n\`\`\``))(P.eof)), n: { v: P.string(`\n\`\`\`\n`), n: null } }))))((__$PH20__ => Applicative.Parser_23f791debc71d215453d5deb93f41ac8.ap()(__$PH20__)(Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(String.fromList)(P.manyTill(P.anyChar)(P.lookAhead(P.string(`\n\`\`\``))))))((__$PH19__ => apL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Applicative.Parser_23f791debc71d215453d5deb93f41ac8)(__$PH19__)(P.char(__String.fromCharCode(10))))((__$PH18__ => Applicative.Parser_23f791debc71d215453d5deb93f41ac8.ap()(__$PH18__)(Alternative.Parser_23f791debc71d215453d5deb93f41ac8.alt()(Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(String.fromList)(P.letters))(Applicative.Parser_23f791debc71d215453d5deb93f41ac8.pure()(``))))(mapL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)((lang => c => Code(lang)(c)))(_P_))))))(P.string(`\`\`\``));
let blockquote = (_P_ => (__$PH23__ => apL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Applicative.Parser_23f791debc71d215453d5deb93f41ac8)(__$PH23__)(P.choice(({ v: doubleReturnTerminal, n: { v: P.lookAhead(P.string(`\n\`\`\``)), n: { v: P.lookAhead(P.string(`\n>`)), n: null } } }))))((__$PH22__ => Applicative.Parser_23f791debc71d215453d5deb93f41ac8.ap()(__$PH22__)(contentWithLineReturn(P.choice(({ v: P.string(`\n`), n: { v: P.string(`\`\`\``), n: { v: P.string(`>`), n: null } } })))))(mapL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Blockquote)(_P_))))(Alternative.Parser_23f791debc71d215453d5deb93f41ac8.alt()(P.symbol(`>`))(P.string(`>`)));
let listItemStart = Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(always(``))(apL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Applicative.Parser_23f791debc71d215453d5deb93f41ac8)(P.many(P.char(__String.fromCharCode(32))))(apL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Applicative.Parser_23f791debc71d215453d5deb93f41ac8)(P.oneOf(({ v: __String.fromCharCode(42), n: { v: __String.fromCharCode(45), n: { v: __String.fromCharCode(43), n: null } } })))(P.some(P.char(__String.fromCharCode(32))))));
let unorderedListItem = (_P_ => Monad.Parser_23f791debc71d215453d5deb93f41ac8.chain()(always(apL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Applicative.Parser_23f791debc71d215453d5deb93f41ac8)(content)(singleReturnTerminal)))(_P_))(listItemStart);
let unorderedList = (_P_ => Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(UnorderedList)(P.some(_P_)))(unorderedListItem);
let paragraph = (_P_ => (__$PH24__ => apL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Applicative.Parser_23f791debc71d215453d5deb93f41ac8)(__$PH24__)(P.choice(({ v: doubleReturnTerminal, n: { v: P.lookAhead(P.string(`\n\`\`\``)), n: { v: P.lookAhead(P.string(`\n>`)), n: { v: P.lookAhead(apL(Functor.Parser_23f791debc71d215453d5deb93f41ac8)(Applicative.Parser_23f791debc71d215453d5deb93f41ac8)(P.string(`\n`))(listItemStart)), n: null } } } }))))(Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(Paragraph)(_P_)))(contentWithLineReturn(P.choice(({ v: listItemStart, n: { v: P.string(`\n`), n: { v: P.string(`\`\`\``), n: { v: P.string(`>`), n: null } } } }))));
let block = P.choice(({ v: heading(H6)(`######`), n: { v: heading(H5)(`#####`), n: { v: heading(H4)(`####`), n: { v: heading(H3)(`###`), n: { v: heading(H2)(`##`), n: { v: heading(H1)(`#`), n: { v: unorderedList, n: { v: blockquote, n: { v: code, n: { v: paragraph, n: null } } } } } } } } } }));
let markdownParser = (_P_ => Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()((_P_ => Functor.List_5b7ebeeaa5acfe1eeea5a9e9845b152d.map()((__x__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let x = __x__.__args[0];
    return x;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__)))(filter((__x__ => ((__x__) => {
  if (__x__.__constructor === "Just" && true) {
    let x = __x__.__args[0];
    return true;
  }
  else if (__x__.__constructor === "Nothing") {
    return false;
  }
  else {
    console.log('non exhaustive patterns for value: ', __x__.toString()); 
    console.trace(); 
    throw 'non exhaustive patterns!';
  }
})(__x__)))(_P_))))(P.many(P.choice(_P_))))(({ v: Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(always(Nothing))(P.spaces), n: { v: Functor.Parser_23f791debc71d215453d5deb93f41ac8.map()(Just)(block), n: null } }));
export let parseMarkdown = (_P_ => mapLeft(always(`Malformed markdown input`))(P.runParser(markdownParser)(_P_)));
export default { parseMarkdown, Text, Bold, Italic, InlineCode, Link, Image, LineReturn, H1, H2, H3, H4, H5, H6, Paragraph, Blockquote, Code, UnorderedList };
