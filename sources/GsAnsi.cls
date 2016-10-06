"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GsAnsi
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GsAnsi guid: (GUID fromString: '{8FB0BE87-DC6B-46EB-BB2A-77101CEF636A}')!
GsAnsi comment: ''!
!GsAnsi categoriesForClass!Kernel-Objects! !
!GsAnsi class methodsFor!

script

"
AnsiShell show.
"
| children parents std x |
std := AnsiStandard readFromPath:  'M:\Documents\Dolphin\ANSI\ANSI.STB'.
children := Dictionary new.
std protocols do: [:each | 
	1 < (x := each conformsTo) size ifTrue: [
		children at: each put: each conformsTo.
	].
].
children.
parents := Dictionary new.
children keysAndValuesDo: [:eachChild :eachParents | 
	eachParents do: [:eachParent |
		(parents at: eachParent ifAbsentPut: [OrderedCollection new]) add: eachChild.
	].
]. 
parents.
! !
!GsAnsi class categoriesFor: #script!public! !

