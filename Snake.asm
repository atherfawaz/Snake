org 100h
;////////////////////////////////////////////////////////////////////////////////////
;Subroutines THUS FAR Implemented inorder
; grow: >increase the size of snake by one char, takes no parameters,
;		can be called N times to increase the size by N chars

; disp_snake:> displays snake
;
; disp_score:> displays score
;									 
; delay:> hmmm......delay I suppose...

; check_valid:> checks whether head is going to a valid place
; shift_body: > shifts the body along, used in conjunction with snake movement subroutines

; snake,up,down,left and right: >move the snake in implied direction

; Overlap: > a versatile subroutine,takes the overlapper, the thing being overlapped and
;			 the size of the overlapped objects as parameters, basically a linear search
;			 which returns a bool value
;			can be used to check if the food to be placed is being overlapped by snake's body
; 			or an obstacle, if the snake's head is overlapping with its body
;			or an obtacle, snake's head is overlapping food.....etc.

; play_sound:> takes two parameters from the stack and plays a sound on the speakers.
;				the first one (bp+6) is the duration for beep
;			   the second one (bp+4) is the frequency of the beep

;kbisr: >overloads WASD keys to trigger movement flags, press Q to close program

;interface:> only contains a heart at the moment :)

;heart:> prints a heart, takes offset on the screen as parameter

;clrscr:> clears screen

;generatefood:> generates a random food position and stores the position in a global variable

;eating:> checks if the snake can eat the food and if it can, ingest the food and grow the snake

;scoreupdating:> if the snake has eaten the food, update the score
;////////////////////////////////////////////////////////////////////////////////////

;///////////////////////////////////////////////////////////////////////////////////
;SUGGESTED Subroutins

; Take_a_life:> a wrapper for Overlap and decrementing lives variable

;Gen_StageN:> will generate a stage with obstacles, Gen_Stage1 can simply be a box....

;Timer(A TSR)

; Speed:> next gen of Delay, optimizes the speed along columns and rows, create a Speed global variable

;Increase_Speed:> change the Speed variable to increase the speed and runs every 5 seconds (hooked with timer maybe)
 
;TO BE CONTINUED.............
;/////////////////////////////////////////////////////////////////////////////////
jmp start

oldisr: dd 0
nos:db 3;no. of stages
;////////FLAGS////////////////
close: db 0
left: db 0
right: db 0
up: db 0
down: db 0
foodplaced: db 0
validfood: db 1
;//////SNAKE-PARAMETERS/////////////////////
reincarnation:dw 1680,1682,1684,1686,1688,1690,1692,1694,1696,1698,1700,0xffff
snake: dw 1680,1682,1684,1686,1688,1690,1692,1694,1696,1698,1700,0xffff; these will be the values of di-offset 
extension:times 228 dw 0
sizeS: dw 11
lives: db 3
Score: dw 0
SpeedConstant: dw 4096
SpeedFlag: db 1
SpeedNum:dw 60
;//////POISON-PARAMETER/////////////////////
poisonpos: dw 0
poisonitem: dw 0x1632		;cursed remains of old snakes, black magic
poisonplaced: db 0
;//////FOOD-PARAMETERS//////////////////////
foodpos: dw 0
fooditem: dw 0x1625	;%
fooditem2: dw 0x2340;@
fooditem3: dw 0x3426;&
fooditem4: dw 0x467E ;~
currentfood: dw 0x1625
cycle: dw 0
;//////AUDIO-PARAMETERS//////////////////////
eatS: dw 4560
deathS: dw 9121
initializationS: dw 1917
initializationT: dw 65000
normalT: dw 1500
;		G D G A A# F6 F5 A A# G6 D F A A# C6 F A A#
notes: dw 3043,4063,3043,2711,2559,1715,3416,2711,2559,1521,4063,3416,2711,2559,2280,3416,2711,2559
note_off: dw 0
;////////////INTERFACE-COMPONENTS//////////////////////////
heartS: dw 0x0720,0x0720,0x1720,0x1720,0x0720,0x0720,0x1720,0x1720,0x0720
		dw 0x0720,0x1720,0x1720,0x1720,0x1720,0x1720,0x1720,0x1720,0x1720
		dw 0x0720,0x0720,0x1720,0x1720,0x1720,0x1720,0x1720,0x1720,0x0720
		dw 0x0720,0x0720,0x0720,0x0720,0x1720,0x1720,0x0720,0x0720,0x0720
Lives: dw 'Lives Remaining'		
Scoredisp: dw 'Score'
time: dw 'Time'
oldtime: dd 0
tickcount: dw 0
sec: dw 0
min: dw 0
bkcol: dw 0x002f;0=blk,1=blu,2=gr,3=tr,4=rd,5=pp,6=or,7=wt
obscol: dw 0x2223
sbod: dw 0x7323
head: dw 0x1032
;///////////SNAKE'S BODY RELATED SUBROUTINES//////////////////////////////////		
;//////////////////GROW///////////////
grow:
	push ax
	push bx
	mov bx,[sizeS]
	shl bx,1
	sub bx,2
	mov ax,[snake+bx]
	add bx,2
	mov [snake+bx],ax
	add bx,2
	mov ax,0xffff
	mov [snake+bx],ax
	mov ax,1
	add [sizeS],ax
	pop bx
	pop ax
	ret

;////////////////////	POISON-E///////////////
poison_E:
		push ax
		push di
		push bx
		push 0xb800
		pop es
		cmp word [sizeS],0
		jb kill
		mov di,[sizeS]
		sub di,5
		mov [sizeS],di
		shl di,1
		mov word [snake+di],0xffff
		mov ax,[bkcol]
		mov bx,[di]
		mov [es:bx],ax
		add di, 2
		add di,snake
		erase:cmp word [di],0xFFFF
				je endpe
				mov ax,[bkcol]
				mov bx,[di]
				mov [es:bx],ax
				add di,2
				jmp erase
		kill:
			dec byte [lives]
		
		endpe:
		mov [es:bx],ax
		pop bx
		pop di
		pop ax
		ret
;//////////////////DISP_SNAKE///////////////		
disp_snake:
	push ax
	push di
	push bx
	mov ax,0xb800
	mov es,ax
	mov ax,[head];head
	mov di,[snake]
	mov bx,2
	mov [es:di],ax
	mov ax,[sbod];body
	loop1:
		mov di,[snake+bx]
		cmp di,0xffff
		je out1
		add bx,2
		mov [es:di],ax
		jmp loop1
		
	out1:
	sub bx,2
	mov di,[snake+bx]
	
	mov bx,di;CHANGED THIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	cmp word [currentfood],bx
	je dontplace
	mov bx,[bkcol]
	mov word [es:di],bx
	dontplace:
	pop bx
	pop di
	pop ax
	ret

;//////////delay///////////////
;////////////SPEED//////////////////	
	delayL:
	push ax
	push bx
	push dx
	push cx
	cmp word [SpeedConstant],0
	je nes
	call incrementspeedL
	nes:
	mov dx,[cs:SpeedConstant]
	
	
	kl:
	mov cx, 0xFF
	loopdelay:
	dec cx
	jnz loopdelay
	dec dx
	cmp dx, 0
	jg kl

	pop cx
	pop dx
	pop bx
	pop ax
	ret
	

incrementspeedL:
	push dx
	push ax
	mov ax, [cs:sec]
	mov dl,20
	div dl
	cmp ah,0
	jne setagain
	
	cmp byte [cs:SpeedFlag],1
	je incrementspeedpops
	
	shr word [cs:SpeedConstant],1
	or byte [cs:SpeedFlag],1
	jmp incrementspeedpops
	
	setagain:
	and byte [cs:SpeedFlag],0
	
	incrementspeedpops:
	pop ax
	pop dx
	ret	
	
;/////////////////////////////////////////////////////////	

		
delay:
	push ax
	push bx
	push dx
	push cx
	
	cmp word [SpeedNum],0
	je nes2
	call incrementspeed
	nes2:
	mov dx,[cs:SpeedNum]
	
	kl2:
	mov cx, 0xfff
	loopdelay2:
	dec cx
	jnz loopdelay2
	dec dx
	cmp dx, 0
	jg kl2

	pop cx
	pop dx
	pop bx
	pop ax
	ret
	
	
incrementspeed:
	push dx
	push ax
	mov ax, [cs:sec]
	mov dl,20
	div dl
	cmp ah,0
	jne setagain1
	
	cmp byte [cs:SpeedFlag],1
	je incrementspeedpops1
	
	sub word [cs:SpeedNum],4
	or byte [cs:SpeedFlag],1
	jmp incrementspeedpops1
	
	setagain1:
	and byte [cs:SpeedFlag],0
	
	incrementspeedpops1:
	pop ax
	pop dx
	ret	
	
;////////////////CHECK_VALID////////////////
check_valid:
		push bp
		mov bp,sp
		push di
		mov di,[bp+4]
		mov ax,[es:di]
		cmp byte al,0x23
		jne validspace
		mov ax,0
		jmp endc
		validspace:
		mov ax,1
		endc:
		pop di
		pop bp
		ret 2
;//////////////////SHIFT_BODY///////////////	
	shift_body:
		mov di,[snake+bx]
		cmp di,0xffff
		je exitu
		mov [snake+bx],si
		add bx,2
		mov si,[snake+bx]
		cmp si,0xffff
		je exitu
		mov [snake+bx],di
		add bx,2
		jmp shift_body
		exitu:
		ret
;//////////////////SNAKEUP///////////////
snakeup:
	push ax
	push di
	push bx
	mov bx,[snake] ; moving the head
	mov di,160
	cmp bx,960
	jnb nowrapu
	add bx,3200
	nowrapu:
	sub bx,di
	push bx
	call check_valid
	cmp ax,0
	je dead1
	mov si,[snake]
	mov [snake],bx
	mov bx,2	

	call shift_body
	jmp endu
	dead1:
	call body_collision
	endu:
	pop bx
	pop di
	pop ax
	ret
;//////////////////SNAKEDOWN///////////////	
snakedown:
	push ax
	push di
	push bx
	mov bx,[snake] ; moving the head
	mov di,160
	cmp bx,3840
	jb nowrapd
	sub bx,3200
	nowrapd:
	add bx,di
	push bx
	call check_valid
	cmp ax,0
	je dead2
	mov si,[snake]
	mov [snake],bx
	mov bx,2	

	call shift_body
	jmp endd
	dead2:
	call body_collision
	endd:
	pop bx
	pop di
	pop ax
	ret
;//////////////////SNAKELEFT///////////////		
snakeleft:
	push di
	push bx
	push ax
	push dx
	mov bx,[snake] ; moving the head
	mov di,2
	mov ax,bx
	xor dx,dx
	mov cx,160
	div cx
	
	cmp dx,0
	jne nowrapl
	add bx,160
	nowrapl:
	sub bx,di
	push bx
	call check_valid
	cmp ax,0
	je dead3
	mov si,[snake]
	mov [snake],bx
	mov bx,2	

	call shift_body
	jmp endl
	dead3:
	call body_collision
	endl:
	pop dx
	pop ax
	pop bx
	pop di
	ret

	
	
;//////////////////SNAKERIGHT///////////////		
snakeright:
	push di
	push bx
	push ax
	push dx
	mov bx,[snake] ; moving the head
	mov di,2
	mov ax,bx
	xor dx,dx
	add ax,2
	mov cx,160
	div cx
	
	cmp dx,0
	jne nowrapr
	sub bx,160
	nowrapr:
	add bx,di
	push bx
	call check_valid
	cmp ax,0
	je dead4
	mov si,[snake]
	mov [snake],bx
	mov bx,2	

	call shift_body
	jmp endr
	dead4:
	call body_collision
	endr:
	pop dx
	pop ax
	pop bx
	pop di
	ret

	
;///////////////////BODY_COLLISION	///////////////////
del:
	push cx
	mov cx,0xf000
	a:
	sub cx,1
	jnz a
	pop cx 
	ret
body_collision:
	push cx
	push si
	push di
	push ax
	push bx
	push dx
	
		mov bx,snake
		mov cx,[sizeS]
		mov ax,[bkcol]
		loopi:
			mov di,[bx]
			mov [es:di],ax
			add bx,2
			call del
			loop loopi
		
		mov cx,12
		push cs
		pop es
		mov si,reincarnation
		mov di,snake
		rep movsw
		mov cx,11
		mov [sizeS],cx
		push word 0xb800
		pop es
		and byte [up],0
		and byte [down],0
		and byte [right],0
		and byte [left],0
		push word [initializationT]
		push word [deathS]
		call play_sound
		call disp_snake
		mov ax,1
		sub [lives],al
		call disp_life
	
	its_fine:
	pop dx
	pop bx
	pop ax
	pop di
	pop si
	pop cx
	ret
;/////////////////////OVERLAP///////////////////

;overlap:
;		push bp
;		mov bp,sp
;		push cx
;		push bx
;		mov cx,[bp+4];size of the array
;		mov bx,[bp+6];array
;		mov ax,[bp+8];key
;		loops:
;			cmp ax,[bx]
;			je exits
;			add bx,2
;			loop loops
;			jmp nf
;		exits:
;		mov ax,1
;		jmp endo
;		nf:
;		mov ax,0
;		endo:
;		pop bx
;		pop cx
;		pop bp
;		ret 6
		
;///////////////ISRs and TSRs//////////////////////////	
;//////////////////KBISR///////////////	
kbisr:
	push ax
	push es
	mov ax, 0xb800
	mov es, ax
	in al, 0x60 
		mov ah,1
		cmp [right],ah
		je cmpright
		cmp al, 30
		jne cmpright
		mov al,1
		mov byte [left],al
		mov al,0
		mov byte[right],al
		mov byte[up],al
		mov byte[down],al
		jmp nomatch
	
	
	cmpright:
		cmp [left],ah
		je cmpup
		
		cmp al, 32 
		jne cmpup
		mov al,1
		mov byte [right],al
		mov al,0
		mov byte[left],al
		mov byte[up],al
		mov byte[down],al
		jmp nomatch
	
	
	
	cmpup:
		cmp [down],ah
		je cmpdown
		
		cmp al, 17
		jne cmpdown
		mov al,1
		mov byte [up],al
		mov al,0
		mov byte[right],al
		mov byte[left],al
		mov byte[down],al
		jmp nomatch
	
	
	
	cmpdown:
		cmp [up],ah
		je terminate
		
		cmp al, 31 
		jne terminate 
		mov al,1
		mov byte [down],al
		mov al,0
		mov byte[right],al
		mov byte[up],al
		mov byte[left],al
		jmp nomatch
	
	terminate:
		cmp al, 16
		jne nomatch 
		or byte [close],1
		;call poison_E
		
		
	nomatch: mov al, 0x20
	out 0x20, al ; send EOI to PIC
	pop es
	pop ax
	iret


;/////////////////AUDIO-IMPLEMENTATIONS/////////////////////
play_sound:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx

	mov al, 182
	out 43h, al
	mov ax, [bp+4]	;move the passed frequency

	out 42h, al
	mov al, ah
	out 42h, al
	in al, 61h

	or al, 00000011b
	out 61h, al
	mov bx, 25
;pause1:
 ;   mov cx, [bp+6]
;pause2:
 ;   dec cx
  ;  jne pause2
   ; dec bx
    ;jne pause1
    ;in al, 61h
    ;and al, 11111100b
    ;out 61h, al

	pop cx
	pop bx
	pop ax
	pop bp
	ret 4

;//////////////////GRAPHICAL-IMPLEMENTATIONS/////////////////////

;///////////////////GEN-STAGE-1//////////////////////////////////////
gen_s1:
		push ax
	mov word	[bkcol], 0x772f;0=blk,1=blu,2=gr,3=tr,4=rd,5=pp,6=or,7=wt
	mov word	[obscol], 0x2223
	mov word	[sbod], 0x5223
	mov word	[head], 0x1032
		call clrscr
		push word [obscol]
		push word 800
		push word 79
		push word 20
		call rectangle
		
		mov ax,[bkcol]
	or ah,0x0f
		or byte [currentfood+1],0xf0
		and [currentfood+1],ah
		or byte [fooditem+1],0xf0
		and [fooditem+1],ah
		or byte [fooditem2+1],0xf0
		and [fooditem2+1],ah
		or byte [fooditem3+1],0xf0
		and [fooditem3+1],ah
		or byte [fooditem4+1],0xf0
		and [fooditem4+1],ah
		pop ax
		ret

;///////////////////LINE/////////////////////////////////////////
line:
		push bp
		mov bp,sp
		push ax
		push cx
		push di
		mov di,[bp+8];ini
		mov cx,[bp+6];fin
		mov ax,[bp+10];col
		loopll:
			mov [es:di],ax
			add di,[bp+4]
			cmp di,cx
			jne loopll
		pop di
		pop cx
		pop ax
		pop bp
		ret 8
;///////////////////GEN-STAGE-2//////////////////////////////////////
gen_s2:
		push ax
	mov word	[bkcol], 0x222f;0=blk,1=blu,2=gr,3=tr,4=rd,5=pp,6=or,7=wt
	mov word	[obscol], 0x7723
	mov word	[sbod], 0x5223
	mov word	[head], 0x1032
		call clrscr
		push word [obscol]
		push word 800
		push word 79
		push word 20
		call rectangle
		
		mov ax,[bkcol]
	or ah,0x0f
		or byte [currentfood+1],0xf0
		and [currentfood+1],ah
		or byte [fooditem+1],0xf0
		and [fooditem+1],ah
		or byte [fooditem2+1],0xf0
		and [fooditem2+1],ah
		or byte [fooditem3+1],0xf0
		and [fooditem3+1],ah
		or byte [fooditem4+1],0xf0
		and [fooditem4+1],ah
		
		push word [obscol]
		push word 1440
		push word 1560
		push word 2
		call line
		push word [bkcol]
		push word 3840
		push word 3900
		push word 2
		call line
		push word [bkcol]
		push word 840
		push word 870
		push word 2
		call line
		push word [bkcol]
		push word 1600
		push word 160*18
		push word 160
		call line
		push word [bkcol]
		push word 1758
		push word 160*19-2
		push word 160
		call line
		push word [obscol]
		push word 920
		push word 3960
		push word 160
		call line
		pop ax
		ret		
;//////////////////INTERFACE///////////////	
	interface:
	push cx
	push di
	mov di,640
	push word 0xb800
	pop es
	push ax
	mov ax,0x3420
	mov cx,80
	rep stosw
	call disp_life
	call disp_score
	pop ax
	pop di
	pop cx
	ret

;////////////////DISP_SCORE//////////////
printsc:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di

	mov ax, 0xb800
	mov es, ax
	mov ax, [bp+4]
	mov bx, 10
	mov cx, 0

nextdigit:
	xor dx, dx
	div bx
	add dl, 0x30
	push dx
	inc cx
	cmp ax, 0
	jnz nextdigit
	xor di, di
	mov di, [bp + 6]
nextpos:
	pop dx
	mov dh, 0x07
	mov word [es:di], dx
	add di, 2
	loop nextpos

	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 4

disp_score:
	push si
	push cx
	push di
	push ax

	mov di, 280
	mov si, Scoredisp
	mov cx, 5
	scloop:
	mov ax, [si]
	mov ah, 0x07
	stosw
	add si, 1
	loop scloop
	
	add di, 2
	push di
	mov ax, [Score]
	push ax
	call printsc

	pop ax
	pop di
	pop cx
	pop si
	ret 

;////////////////DISP_LIFE//////////////
disp_life:
	push si
	push cx
	push di
	mov di,440
	mov si,Lives
	mov cx,15
	loopl:
	mov ax,[si]
	mov ah,0x07
	stosw
	add si,1
	loop loopl
	mov cx,[lives]
	add cx,0x30
	mov ch,0x07
	add di,2
	mov [es:di],cx
	pop di
	pop cx
	pop si
	ret


;////////////RECTANGLE////////////////
rectangle:
	push bp
	mov bp,sp
	push ax
	push di
	push si
	push cx
	push dx
	mov dx,[bp+10]
	mov ax,0xb800
	mov es,ax
	shl word [bp+6],1
	mov ax,[bp+4]
	sub ax,1
	mov [bp+4],ax
	mov di,[bp+8]
	mov ax,[bp+8]
	mov cx,[bp+6]
	add [bp+6],ax
	loopre:
		mov word [es:di],dx
		add di,2
		cmp di,[bp+6]
		jne loopre
		mov word [es:di],dx
	mov di,1
	mov ax,160
	add ax,[bp+8]
	mov [bp+6],cx
	loopwe:
		mov si,ax
		mov word [es:si],dx
		add si,[bp+6]
		mov word [es:si],dx
		mov si,ax
		add di,1
		add ax,160
		cmp di,[bp+4]
		jne loopwe
	mov di,[bp+4]
	mov ax,160
	mul di
	mov di,ax
	add ax,[bp+6]
	add ax,[bp+8]
	add di,[bp+8]
	mov dx,[bp+10]
	loopde:
		mov word [es:di],dx
		add di,2
		cmp di,ax
		jne loopde
	mov word [es:di],dx
	
	pop dx
	pop cx
	pop si
	pop di
	pop ax
	pop bp
	ret 8	
;//////////////////HEART///////////////	
heart:; parameter will be the base offset
	push bp
	mov bp,sp
	mov ax,[bp+4]
	mov si,heartS
	mov di,ax
	mov cx,9
	rep movsw
		
	mov di,ax
	add di,160
	mov cx,9
	rep movsw
	
	mov di,ax
	add di,320
	mov cx,9
	rep movsw
	
	mov di,ax
	add di,480
	mov cx,9
	rep movsw
	pop bp 
	ret 2
		
;//////////////////CLEARSCREEN///////////////		
clrscr: 	
	push es
	push ax
	push cx
	push di	
	mov ax, 0xb800
	mov es, ax ; point es to video base
	xor di, di ; point di to top left column
	mov ax, [bkcol] ; space char in normal attribute
	mov cx, 2000 ; number of screen locations
	cld ; auto increment mode
	rep stosw ; clear the whole screen
	xor di, di ; point di to top left column
	mov ax, 0x720 ; space char in normal attribute
	mov cx,320 ; number of screen locations
	cld ; auto increment mode
	rep stosw ; clear the whole screen
	
	pop di	
	pop cx
	pop ax
	pop es
	ret

;//////////////////SCORING///////////////
scoreupdate:
	push ax
	mov ax, [currentfood]
	cmp ax, 0x725
	je f1
	cmp ax, 0x740
	je f2
	cmp ax, 0x726
	je f3
	add word [Score], 20
	jmp scoreupdatepops
	f1:
	add word [Score], 5
	jmp scoreupdatepops
	f2:
	add word [Score], 10
	jmp scoreupdatepops
	f3:
	add word [Score], 15
	jmp scoreupdatepops
	scoreupdatepops:
	pop ax
	ret
;//////////////////POISON/////////////
poisonplacement:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push di
	push es

	;removing old poison
	mov di, [poisonpos]
	mov ax, 0xb800
	mov es, ax
	mov dx, [bkcol]
	mov word [es:di], dx

	;adding new poison
	mov bx, [foodpos]
	sub bx, 4
	mov word [poisonpos], bx
	push bx
	call check_valid
	cmp ax, 1
	je addpoisonnow

	poisonplacementpops:
	pop es
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret

	addpoisonnow:
	mov di, [poisonpos]
	mov ax, [poisonitem]
	mov word [es:di], ax
	mov byte [poisonplaced], 1
	jmp poisonplacementpops

;//////////////////FOOD///////////////	
chkfoodpos:
	push bp
	mov bp, sp
	push cx
	push ax
	push si
	push dx
	push bx

	mov ax, 0xb800
	mov es, ax
	xor cx, cx
	mov dx, [foodpos]
	mov si, dx
	cmp dx, 1000
	jbe invalidpos

foodbelow:
	mov bx, [es:si]
	cmp bx, [bkcol]
	jne invalidpos

valid:
	mov byte [validfood], 1
	jmp foodpops

invalidpos:
	mov byte [validfood], 0

foodpops:
	pop bx
	pop dx
	pop si
	pop ax
	pop cx
	pop bp
	ret

foodplacement:
	push bp
	mov bp, sp
	push ax
	push cx
	push dx
	push di
	push bx
	push es

generatefood:
	;mov ah, 00h	;set AH to access 00 of 1A interrupt
	;int 1Ah		;get timer values
	mov ax, [sizeS]
	mov dx,[tickcount]
	add dx,[snake]
	xor dl,dh
	xor ax,dx
	not dx
	mul dl
	xor dx, dx
	xor di, di
	mov bx, 0xb800
	mov es, bx
	mov bx, 2000
	div bx
	inc dx
	and dx, 0xFFFE
	mov di, dx
	add di, 840
	mov word [foodpos], di
	call chkfoodpos
	cmp byte [validfood], 1
	jne generatefood

	call poisonplacement

	;generate different foods
	mov ax, [snake]
	xor dx, dx
	mov cx, 20
	div cx
	;remainder now in dx
	cmp dx, 5
	jbe foodone
	cmp dx, 10
	jbe foodtwo
	cmp dx, 14
	jbe foodthree
	mov bx, [fooditem4]
	mov word [currentfood], bx
	jmp contpo

	foodone:
	mov bx, [fooditem]
	mov word [currentfood], bx
	jmp contpo

	foodtwo:
	mov bx, [fooditem2]
	mov word [currentfood], bx
	jmp contpo

	foodthree:
	mov bx, [fooditem3]
	mov word [currentfood], bx
	jmp contpo

	contpo:
	mov word [es:di], bx
	mov byte [foodplaced], 1

	pop es
	pop bx
	pop di
	pop dx
	pop cx
	pop ax
	pop bp
	ret

;//////////////////EATING///////////////
eating:
	push bp
	mov bp, sp
	push ax
	push dx

	mov ax, [snake]
	mov dx, [foodpos]
	cmp ax, dx
	jne poisoncheck
	mov byte [foodplaced], 0
	mov byte [validfood], 1
	call grow
	call grow
	call grow
	call grow
	call scoreupdate
	push word [normalT]
	push word [eatS]
	call play_sound
	call foodplacement
	call disp_score

	poisoncheck:
	mov dx, [poisonpos]
	cmp ax, dx
	jne eatingpops

	;eatpoison
	dec byte [lives]
	call disp_life
	mov byte [poisonplaced], 0
	mov byte [foodplaced], 0
	mov ax, [bkcol]
	mov di, [foodpos]
	mov word [es:di], ax
	call foodplacement
	cmp word [Score], 0
	je eatingpops
	sub word [Score], 5
	call disp_score

eatingpops:
	pop dx
	pop ax
	pop bp
	ret
;/////////////TIMER////////////////
printnum: push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push di
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax
mov bx, 10 ; use base 10 for division
mov cx, 0 ; initialize count of digits
nextdigit1: mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30 ; convert digit into ascii value
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit1 ; if no divide it again
mov di, [bp+6]
nextpos1: pop dx ; remove a digit from the stack
mov dh, 0x07 ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop nextpos1 ; repeat for all digits on stack
pop di
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 4
; timer interrupt service routine
tick_it:
		cmp word [cs:sec],60
		jne nottimeyet
		inc word[cs:min]
		and word [cs:sec],0
		nottimeyet:
		ret

single_digit:
			cmp word [cs:sec],10
			jnb notsingle
			push word 0xb800
			pop es
			mov word [es:142],0x720
			notsingle:
			ret
timer: push ax
		push bx

mov     al, 182         ; Prepare the speaker for the
        out     43h, al         ;  note.
		mov bx,[cs:note_off]
        mov     ax, [cs:notes+bx]        ; Frequency number (in decimal)
                                ;  for middle C.
        out     42h, al         ; Output low byte.
        mov     al, ah          ; Output high byte.
        out     42h, al 
		
		mov ax,[cs:tickcount]
		mov cl,6
		div cl
		
	cmp ah,0
		jne rests
    play:    in      al, 61h         ; Turn on note (get value from
                                ;  port 61h).
        or      al, 00000011b   ; Set bits 1 and 0.
        out     61h, al         ; Send new value.
        add     bx, 2          ; Pause for duration of note.
		cmp bx,36
		jne skipbb
		mov bx,0
		skipbb:
		mov [cs:note_off],bx
		jmp nexto
		
    rests:  ;  in      al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        ;and     al, 11111100b   ; Reset bits 1 and 0.
        ;out     61h, al         ; Send new value.
	
	nexto:


	inc word [cs:tickcount]; increment tick count
	cmp word [cs:tickcount],18
	jne exitt
	inc word [cs:sec]
	call tick_it
	push word 140
	push word [cs:sec]
	call printnum ; print tick count
	call single_digit
	push word 136
	push word [cs:min]
	call printnum ; print tick count
	mov ax,0
	mov [cs:tickcount],ax
	exitt:mov al, 0x20
	out 0x20, al ; end of interrupt
	pop bx
	pop ax
	iret ; return from interrupt
;/////////////////////MAIN/////////////////////////////
start:
	;call gen_s1
	mov ax,[bkcol]
	or ah,0x0f
		or byte [currentfood+1],0xf0
		and [currentfood+1],ah
		or byte [fooditem+1],0xf0
		and [fooditem+1],ah
		or byte [fooditem2+1],0xf0
		and [fooditem2+1],ah
		or byte [fooditem3+1],0xf0
		and [fooditem3+1],ah
		or byte [fooditem4+1],0xf0
		and [fooditem4+1],ah
	call clrscr
	call interface
	call disp_life
	xor ax, ax
	mov es, ax ; point es to IVT base
	mov ax, [es:9*4]
	mov [oldisr], ax ; save offset of old routine
	mov ax, [es:9*4+2]
	mov [oldisr+2], ax ; save segment of old routine
	
	mov ax, [es:8*4]
	mov [oldtime], ax ; save offset of old routine
	mov ax, [es:8*4+2]
	mov [oldtime+2], ax ; save segment of old routine
	

	cli ; disable interrupts
	mov word [es:9*4], kbisr ; store offset at n*4
	mov [es:9*4+2], cs ; store segment at n*4+2
	
	mov word [es:8*4], timer; store offset at n*4
	mov [es:8*4+2], cs ; store segment at n*4+2
	
	call disp_snake
	sti ; enable interrupts
	push word [initializationT]
	push word [initializationS]	;play initialization sound
	;call play_sound
	call foodplacement
	call disp_score
	l1:

	cmp byte [up],1
	jne skipu
		call eating
		call snakeup	
		call delayL
		call disp_snake
		
	skipu:
	
	cmp byte [left],1
	jne skipl
		call eating
		call snakeleft	
		call delayL
		call disp_snake
		
	skipl:
	
	cmp byte [right],1
	jne skipr
		call eating
		call snakeright	
		call delayL
		call disp_snake
		
	skipr:
	cmp byte [down],1
	jne skipd
		call eating
		call snakedown	
		call delayL
		call disp_snake
	
	skipd:
	inc word[cycle]
	cmp word [sizeS],40
	ja clse
	cmp byte [lives],0
	je clse
	
	cmp word [min],4
	jne skipt
	and word [min],0
	and word [sec],0
	dec word [lives]
	call disp_life
	skipt:
	cmp byte [close],0
	je l1
	
	clse:;////////////////CLOSE///////////////////////	
	cmp byte [nos],2
	jne rc
	call body_collision
	call gen_s2
	call interface
	inc byte [lives]
	call disp_snake
	call disp_life
	call foodplacement
	dec byte [nos]
	jmp l1
	rc:
		cmp byte [nos],3
	jne rs
	call body_collision
	call gen_s1
	call interface
	inc byte [lives]
	call disp_snake
	call disp_life
	call foodplacement
	dec byte [nos]
	jmp l1
	rs:
	xor ax,ax
	mov es,ax;///////////NOTE	!!!	not going to clear the es in various functions
	
	mov ax, [oldisr] ; read old offset in ax
	mov bx, [oldisr+2] ; read old segment in bx
	cli ; disable interrupts
	mov [es:9*4], ax ; restore old offset from ax
	mov [es:9*4+2], bx ; restore old segment from bx
	sti ; enable interrupts
	
	mov ax, [oldtime] ; read old offset in ax
	mov bx, [oldtime+2] ; read old segment in bx
	cli ; disable interrupts
	mov [es:8*4], ax ; restore old offset from ax
	mov [es:8*4+2], bx ; restore old segment from bx
	sti ; enable interrupts
	  in      al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and     al, 11111100b   ; Reset bits 1 and 0.
        out     61h, al         ; Send new value.
	mov ax, 0x4c00 ; terminate program
	int 0x21