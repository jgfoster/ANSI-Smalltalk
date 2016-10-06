"Filed out from Dolphin Smalltalk 7"!

AnsiComponent subclass: #AnsiParameter
	instanceVariableNames: 'protocols isCaptured'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AnsiParameter guid: (GUID fromString: '{3A448A3C-4554-4DAD-9B24-8C7CCF62530F}')!
AnsiParameter comment: ''!
!AnsiParameter categoriesForClass!Unclassified! !
!AnsiParameter methodsFor!

initialize: aString

	| list |
	list := aString subStrings.
	name := list first.
	isCaptured := #(true false nil) at: (#('captured' 'uncaptured' 'unspecified') indexOf: list last).
	list size = 3 ifTrue: [
		protocols := Array with: (list at: 2).
	] ifFalse: [list size = 5 ifTrue: [
		protocols := Array with: (list at: 2) with: (list at: 4).
	] ifFalse: [
		self halt.
	]].
!

isCaptured

	^isCaptured.
!

isCapturedString

	isCaptured isNil ifTrue: [^'???'].
	^isCaptured
		ifTrue: ['Yes']
		ifFalse: ['No'].

!

section: aString

	section := aString.
!

typesString

	^protocols size = 1
		ifTrue: [protocols first]
		ifFalse: [protocols first , ' | ' , protocols last].
! !
!AnsiParameter categoriesFor: #initialize:!public! !
!AnsiParameter categoriesFor: #isCaptured!public! !
!AnsiParameter categoriesFor: #isCapturedString!public! !
!AnsiParameter categoriesFor: #section:!public! !
!AnsiParameter categoriesFor: #typesString!public! !

