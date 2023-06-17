[org 0x0100]

jmp start

m_height: dw 7
s_height: dw 4
s_width: dw 20
oldisr: dd 0
fishposition: dw 3240

tickcount: dw 0
oldtimer: dd 0
greentime: dw 29
redtime: dw 14
randomgreen: dw 1000
randomred: dw 1250
greenposition: dw 3880
redposition: dw 2900
score: dw 0
maxlength: dw 80
name: times 81 db 0

msg1: 		db 'Enter Name $'
msg2:       db ' Hey '
msg3:       db ' Welcome to Our Game '
msg4:       db ' FISHDOM '
msg5:       db ' Developers: '
msg6:       db ' Talha Iftikhar 21L-7522 '
msg7:       db ' Fawaz Najam 21L-5415 '
msg8:       db ' Instructions: '
msg8a:      db ' W to move the fish UP '
msg8b:      db ' A to move the fish LEFT '
msg8c:      db ' S to move the fish DOWN '
msg8d:      db ' D to move the fish RIGHT '
msg10:       db ' Press Enter to play the Game or Escape to Exit '
msg11:       db ' Are you sure you want to exit?(Y/N) '
msg12:       db '                                     '

font: times 256*16 db 0 ; space for font
temp: dw 0

background:
	push es
	push ax
	push di

	mov ax, 0xb800 
	mov es, ax 
	mov di, 0

    sky: 
	    mov word [es:di], 0x3020
	    add di, 2 
	    cmp di, 4000 
	    jne sky 

    mov di, 1600

    sea:
	    mov word [es:di], 0x1020 
	    add di, 2 
	    cmp di, 4000 
	    jne sea 

	pop di
	pop ax
	pop es
	ret

mountains:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push es
	push di

	mov ax, 0xb800 
	mov es, ax 
	mov di, 492

mountain:
	mov cx, [bp+4]
	mov dx, 2
	mov ax, 2
	mov word [es:di], 0x6020
nextline:
	dec cx
	jz nextmountain
	
	sub di, ax
	add ax, 4
	add di, 160
	mov bx, 1
	add dx, 2
print:
	mov word [es:di], 0x6020
	inc bx
	cmp dx, bx
	je nextline
	add di, 2
	jmp print

nextmountain:
	mov ax, di
	cmp ax, 1592
	jge mountaindone
	sub di, 946
	jmp mountain

mountaindone:
	pop di
	pop es
	pop dx
	pop cx	
	pop bx
	pop ax
	pop bp
	ret 2

ships:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push es
	push di

	mov ax, 0xb800 
	mov es, ax 
	mov di, 2254

ship:
	mov ax, [bp+6]
	mov dx, ax
	add dx, ax
	sub dx, 2
	mov cx, [bp+4]
	mov bx, 0

printship:
	mov word [es:di], 0x7020
	add di, 2
	inc bx
	cmp ax,bx
	je nextline_s
	jmp printship

nextline_s:
	sub ax, 2
	mov bx, 0
	dec cx
	jz nextship
	add di, 160
	sub di, dx
	sub dx, 4
	jmp printship

nextship:
	mov bx, [bp+6]
	dec bx
	mov [bp+6], bx
	mov bx, [bp+4]
	dec bx
	cmp bx, 2
	je shipsdone
	mov [bp+4], bx
	sub di, 776
	jmp ship

shipsdone:
	pop di
	pop es
	pop dx
	pop cx	
	pop bx
	pop ax
	pop bp
	ret 4

rotateLeft:

	push ax
	push bx
	push cx
	push si	
	push di
	push es
	push ds
	mov ax, 0xb800
	mov es, ax
	mov ds, ax
	mov si, 162
	mov di, 160
	mov bx, 9
	

	lineRotateLeft:
		mov cx, 79
		lodsw
		push ax
		sub si, 2
		cld
		rep movsw
		pop ax
		mov [es:di], ax
		add si, 2
		add di, 2
		call delay
		dec bx
		jnz lineRotateLeft
	
	rotatePopLeft:
		pop ds
		pop es
		pop di
		pop si
		pop cx
		pop bx
		pop ax
		ret

rotateRight:

	push ax
	push bx
	push cx
	push si	
	push di
	push es
	push ds
	mov ax, 0xb800
	mov es, ax
	mov ds, ax
	mov si, 2878
	mov di, 2878
	mov bx, 8
	

	lineRotateRight:
		mov cx, 79
		cld
		lodsw
		push ax
		sub si, 4
		std
		rep movsw
		pop ax
		mov [es:di], ax
		sub di, 2
		dec bx
		call delay
		jnz lineRotateRight

	rotatePopRight:
		pop ds
		pop es
		pop di
		pop si
		pop cx
		pop bx
		pop ax
		ret

delay:                      
	push cx  ;for delay until loop 2
      mov cx, 0x0F00

	loop1:                     
		loop loop1
		mov cx, 0x0F00
	loop2:                     
		loop loop2
		mov cx, 0x0F00
            pop cx
       ret

printfish:
    push bp
    mov bp,sp
	push es
    push di
	push ax

	mov ax,0xb800
	mov es,ax

    mov di, [bp+4]
	mov word [es:di],0x0720

	pop ax
    pop di
	pop es
    pop bp
	ret 2

clearprev:
    push bp
    mov bp, sp
    push es
    push di
    push ax

    mov ax, 0xb800
    mov es, ax

    mov di, [bp+4]
    mov word [es:di], 0x1020

    pop ax
    pop di
    pop es
    pop bp
    ret 2    

fishLeft:
	push di
    push ax
    push bx
    push dx

    mov ax, [cs:fishposition]
	mov di, ax
	push di
    call clearprev

	xor dx, dx
	mov bx, 160
	div bx
	cmp dx, 0
	jne sameLineL
	add di, 160

	sameLineL:
		sub di, 2
		push di
		call printfish
		mov [cs:fishposition], di   

	pop dx
	pop bx
	pop ax
	pop di
	ret


fishRight:
	push di
    push ax
    push bx
    push dx

    mov ax, [cs:fishposition]
	mov di, ax
	push di
    call clearprev

	xor dx, dx
	mov bx, 160
	add ax, 2
	div bx
	cmp dx, 0
	jne sameLineR
	sub di, 160

	sameLineR:
		add di, 2
		push di
		call printfish
		mov [cs:fishposition], di   

	pop dx
	pop bx
	pop ax
	pop di
	ret

fishUp:
	push di

    mov di, [cs:fishposition]
	sub di, 160
	cmp di, 2880
	jl collision

	push di
	call printfish
	mov [cs:fishposition], di   
	add di, 160
	push di
    call clearprev
	jmp UpDownDone

fishDown:
	push di

    mov di, [cs:fishposition]
	add di, 160
	cmp di, 3998
	jg collision

	push di
	call printfish
	mov [cs:fishposition], di   
	sub di, 160
	push di
    call clearprev
    jmp UpDownDone

collision:
    call playSound

UpDownDone: 
	pop di
	ret

delaySound: 
	push cx
	mov cx,0xffff
	loopsound: loop loopsound
	pop cx
	ret

playSound:	
	 push ax
    push cx
   	mov cx, 5
	soundLoop:        
		mov al, 0b6h
		out 43h, al

		;load the counter 2 value for d3
		mov ax, 1fb4h
		out 42h, al
		mov al, ah
		out 42h, al

		;turn the speaker on
		in al, 61h
		mov ah,al
		or al, 3h
		out 61h, al
		call delaySound
		mov al, ah
		out 61h, al

		call delaySound

		;load the counter 2 value for a3
		mov ax, 152fh
		out 42h, al
		mov al, ah
		out 42h, al

		;turn the speaker on
		in al, 61h
		mov ah,al
		or al, 3h
		out 61h, al
		call delaySound
		mov al, ah
		out 61h, al

		call delaySound

		;load the counter 2 value for a4
		mov ax, 0A97h
		out 42h, al
		mov al, ah
		out 42h, al

		;turn the speaker on
		in al, 61h
		mov ah,al
		or al, 3h
		out 61h, al
		call delaySound
		mov al, ah
		out 61h, al

		call delaySound

		loop soundLoop

    pop cx
    pop ax
    ret


kbisr: 
	push ax 
	push es 
	mov ax, 0xb800 
	mov es, ax ; point es to video memory 
	in al, 0x60 ; read a char from keyboard port 
	cmp al, 0x1e ; is the key 'A'
	jne nextCmpRight ; no, try next comparison 
	call fishLeft
    ;call printalpha
	jmp exitmatch
	;mov byte [es:0], 'L' ; yes, print L at top left 
	;jmp nomatch ; leave interrupt routine 
	nextCmpRight: 
		cmp al, 0x20 ; is the 'D' 
		jne nextCmpUp ; no, leave interrupt routine 
		call fishRight
		;call printalphb
		jmp exitmatch
		;mov byte [es:0], 'R' ; yes, print R at top left 
		;jmp nomatch
	nextCmpUp:
		cmp al, 0x11 ; is the key 'W' 
		jne nextCmpDown ; no, leave interrupt routine 
		call fishUp
		jmp exitmatch
		;mov byte [es:0], 'R' ; yes, print R at top left 
		;jmp nomatch
	nextCmpDown:
		cmp al, 0x1f ; is the key 'S' 
		jne nomatch ; no, leave interrupt routine 
		call fishDown
		jmp exitmatch
		;mov byte [es:0], 'R' ; yes, print R at top left 
		;jmp nomatch
	nomatch: 
		;mov al, 0x20 
		;out 0x20, al ; send EOI to PIC 
		pop es 
		pop ax 
		jmp far [cs:oldisr]
		;iret		
	exitmatch:
		mov al, 0x20 
		out 0x20, al ; send EOI to PIC 
		pop es 
		pop ax 
		iret
timer:	
	push ax
	push bx	
	push cx
	push dx
	push di


	inc word[cs:tickcount]
	add word[cs:randomgreen],190
	add word[cs:randomred],239
	mov cx,word[cs:tickcount]

	call printscore

	cmp cx,6; 1/3 times a second
	jne greencoin

	call rotateLeft
	call rotateRight

	;-------------Moving landscape every 1/3 second-------------
	;----------------------------------------------------------

	mov word[cs:tickcount],0;reset 1/3rd second

	redcoin:
		inc word[cs:redtime]
		inc word[cs:greentime]
		mov cx,[cs:redtime]

		cmp cx,15
		jne greencoin

		mov ax,[cs:redposition]
		push ax
		call clearprev

		mov word[cs:redtime],0
		xor dx,dx
		mov bx,1120
		mov ax,[cs:randomred]
		div bx
		add dx,2880
		mov [cs:redposition],dx

		mov ax,0x4030
		push dx
		push ax
		call printcoin

	greencoin:

		mov cx,[cs:greentime]
		cmp cx,30
		jne termination

		mov ax,[cs:greenposition]
		push ax
		call clearprev

		mov word[cs:greentime], 0 ; reset time for green coin

		xor dx,dx
		mov bx,1160
		mov ax,[cs:randomgreen]
		div bx
		add dx,2880
		mov [cs:greenposition],dx

		mov ax,0x2030
		push dx
		push ax
		call printcoin

	termination:
		mov al, 0x20
		out 0x20, al ; end of interrupt
	
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	iret  ;return from interrupt

printcoin:
	push bp
	mov bp, sp
	push es
	push ax
	push di

	mov ax, 0xb800
	mov es, ax				; point es to video base
	xor ax, ax
	mov ax, [bp+4]
	mov di, [bp+6]
	stosw

	pop di
	pop ax
	pop es
	pop bp
	ret 4

printscore:
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base

	mov ax, [cs:fishposition]
	mov bx, [cs:greenposition]
	cmp ax, bx
	jne notcaughtgreen

	mov word [cs:greentime], 25
	add word [cs:score], 10
	mov di, [cs:greenposition]
	push di
	call clearprev
	notcaughtgreen:

	mov ax, [cs:fishposition]
	mov bx, [cs:redposition]
	cmp ax, bx
	jne notcaughtred

	mov word [cs:redtime], 14
	add word [cs:score], 50
	mov di, [cs:redposition]
	push di
	call clearprev
	notcaughtred:

	push word[score]
	call printnum

	pop di
	pop dx
	pop cx
	pop bx
	pop ax 
	pop es
	ret 

printnum:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push es
	push di

	mov ax,0xb800
	mov es,ax
	mov ax,[bp+4]

	mov bx, 10 ; use base 10 for division
	mov cx, 0 ; initialize count of digits
	nextdigit2: 
		mov dx, 0 ; zero upper half of dividend
		div bx ; divide by 10
		add dl, 0x30 ; convert digit into ascii value
		push dx ; save ascii value on stack
		inc cx ; increment count of values
		cmp ax, 0 ; is the quotient zero
		jnz nextdigit2 ; if no divide it again
	mov ax, cx
	mov di, 160
	calculatetopright:
		sub di, 2
		loop calculatetopright
	mov cx, ax	
	nextpos2: 
		pop dx ; remove a digit from the stack
		mov dh, 0x30 ; use normal attribute
		mov [es:di], dx ; print char on screen
		add di, 2 ; move to next screen location
		loop nextpos2 ; repeat for all digits on stack

	pop di
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2

intro:
	pusha
	push si
	mov dx, msg1 ; greetings message
	mov ah, 9 ; service 9 – write string
	int 0x21 ; dos services
	mov cx, [maxlength] ; load maximum length in cx
	mov si, name ; point si to start of buffer
	nextchar: 
		mov ah, 1 ; service 1 – read character
		int 0x21 ; dos services
		cmp al, 13 ; is enter pressed
		je ext ; yes, leave input
		mov [si], al ; no, save this character
		inc si ; increment buffer pointer
		loop nextchar ; repeat for next input char
	ext: 
		;mov byte [si], '$' ; append $ to user input
		; mov dx, message ; greetings message
		; mov ah, 9 ; service 9 – write string
		; int 0x21 ; dos services
		; mov dx, buffer ; user input buffer
		; mov ah, 9 ; service 9 – write string
		; int 0x21 ; dos services
	pop si
	popa
	ret

cls:
		push ax
		push cx
		push di
		push es
		mov ax, 0xb800
		mov es, ax
		xor di, di
		mov cx, 2000
		mov ax, 0x0720
		cld
		rep stosw
		pop es
		pop si 
		pop cx
		pop ax
		ret

introdelay:
	push ax
	mov ax, 0x0fff
	delayloop2:
	sub ax, 1
	jnz delayloop2
	
	pop ax
	ret
introdelay2:
	push ax
	mov ax, 0xffff
	delayloop3:
	sub ax, 1
	jnz delayloop3

	mov ax, 0xffff
	delayloop4:
	sub ax, 1
	jnz delayloop4

	mov ax, 0xffff
	delayloop5:
	sub ax, 1
	jnz delayloop5

	pop ax
	ret
bluescreen:
	;call cls
	
	push ax
	push cx
	push di
	push es
	mov ax, 0xb800
	mov es, ax
	xor di, di
	mov ax, 0x3020
	mov cx, 2000
	cld
	bluescreenloop:
		call introdelay
		stosw
		loop bluescreenloop
	pop es
	pop di
	pop cx
	pop ax
	ret

startscreen:
	call bluescreen
	pusha

	mov ah, 0x13		; service 13 - print string

	mov al, 1			; subservice 01 – update cursor 
	mov bh, 0			; output on page 0

	mov bl, 00010111B	; normal attrib
	mov cx, 5			; length of string
	mov dx, 0x0213		; row 10 column 3
	push ds
	pop es				; es=ds segment of string
	mov bp, msg2	; bp = offset of string
	INT 0x10			; call BIOS video service
	call introdelay2

	mov bl, 00011110B	; normal attrib
	mov cx, 20			; length of string
	mov dx, 0x0218	; row 10 column 3
	push ds
	pop es				; es=ds segment of string
	mov bp, name	; bp = offset of string
	INT 0x10			; call BIOS video service
	call introdelay2

	mov bl, 00010111B	; normal attrib
	mov cx, 21		; length of string
	mov dx, 0x0619	; row 10 column 3
	push ds
	pop es				; es=ds segment of string
	mov bp, msg3	; bp = offset of string
	INT 0x10			; call BIOS video service
	call introdelay2

	mov bl, 00011100B	; normal attrib
	mov cx, 9		; length of string
	mov dx, 0x0820	; row 10 column 3
	push ds
	pop es				; es=ds segment of string
	mov bp, msg4	; bp = offset of string
	INT 0x10 ; bios video services
	call introdelay2

	mov bl, 00010111B	; normal attrib
	mov cx, 13		; length of string
	mov dx, 0x153E	; row 10 column 3
	push ds
	pop es				; es=ds segment of string
	mov bp, msg5	; bp = offset of string
	INT 0x10 ; bios video services
	call introdelay2

	mov bl, 00010111B	; normal attrib
	mov cx, 25		; length of string
	mov dx, 0x1634	; row 10 column 3
	push ds
	pop es				; es=ds segment of string
	mov bp, msg6	; bp = offset of string
	INT 0x10 ; bios video services
	call introdelay2

	mov bl, 00010111B	; normal attrib
	mov cx, 22	; length of string
	mov dx, 0x1734	; row 10 column 3
	push ds
	pop es				; es=ds segment of string
	mov bp, msg7	; bp = offset of string
	INT 0x10 ; bios video services
	call introdelay2

	mov bl, 00010111B	; normal attrib
	mov cx, 15		; length of string
	mov dx, 0x0B00	; row 10 column 3
	push ds
	pop es				; es=ds segment of string
	mov bp, msg8	; bp = offset of string
	INT 0x10 ; bios video services
	call introdelay2

	mov bl, 00010111B	; normal attrib
	mov cx, 23		; length of string
	mov dx, 0x0C05	; row 10 column 3
	push ds
	pop es				; es=ds segment of string
	mov bp, msg8a	; bp = offset of string
	INT 0x10 ; bios video services
	call introdelay2

	mov bl, 00010111B	; normal attrib
	mov cx, 25		; length of string
	mov dx, 0x0D05	; row 10 column 3
	push ds
	pop es				; es=ds segment of string
	mov bp, msg8b	; bp = offset of string
	INT 0x10 ; bios video services
	call introdelay2

	mov bl, 00010111B	; normal attrib
	mov cx, 25		; length of string
	mov dx, 0x0E05	; row 10 column 3
	push ds
	pop es				; es=ds segment of string
	mov bp, msg8c	; bp = offset of string
	INT 0x10 ; bios video services
	call introdelay2

	mov bl, 00010111B	; normal attrib
	mov cx, 26		; length of string
	mov dx, 0x0F05	; row 10 column 3
	push ds
	pop es				; es=ds segment of string
	mov bp, msg8d	; bp = offset of string
	INT 0x10 ; bios video services
	call introdelay2

	mov bl, 10010111B	; normal attrib
	mov cx, 48	; length of string
	mov dx, 0x120F	; row 10 column 3
	push ds
	pop es				; es=ds segment of string
	mov bp, msg10	; bp = offset of string
	INT 0x10 ; bios video services

	popa
	ret

start:
	call intro
	call startscreen
	introkey:
		mov ah, 0
		int 0x16
		cmp al, 27
		jne nextkey
		jmp exit
		nextkey:
			cmp al, 13
			jne introkey

	
	call background

	mov ax, [m_height]
	push ax
	call mountains

	mov ax, [s_width]
	push ax
	mov ax, [s_height]
	push ax
	call ships

    mov ax, [fishposition]
    push ax
	call printfish
	
	game:
		xor ax, ax
		mov es, ax
		mov ax, [es:9*4] 
    	mov [oldisr], ax ; save offset of old routine 
		mov ax, [es:9*4+2] 
		mov [oldisr+2], ax ; save segment of old routine 

    	mov ax, [es:8*4]
		mov [cs:oldtimer], ax ; save offset of old routine
		mov ax, [es:8*4+2]
		mov [cs:oldtimer+2], ax ; save segment of old routine

		cli
		mov word [es:9*4], kbisr
		mov [es:9*4+2], cs
    	mov word [es:8*4], timer; store offset at n*4
		mov [es:8*4+2], cs ; store segment at n*4+2
		sti

        	l1:
        	in al, 0x60
        	cmp al,0x01
        	jne l1

			mov ax, [oldisr] ; read old offset in ax
    		mov bx, [oldisr+2] ; read old segment in bx
    		cli ; disable interrupts
    		mov [es:9*4], ax ; restore old offset from ax
    		mov [es:9*4+2], bx ; restore old segment from bx
    		sti

    		mov ax, [oldtimer]
    		mov bx, [oldtimer+2]
    		cli
    		mov [es:8*4], ax
    		mov [es:8*4+2], bx
    		sti
			exitkey:
				mov ah, 0x13		; service 13 - print string
				mov al, 1			; subservice 01 – update cursor 
				mov bh, 0			; output on page 0
				mov bl, 00010111B	; normal attrib
				mov cx, 37	; length of string
				mov dx, 0x1314	; row 10 column 3
				push ds
				pop es				; es=ds segment of string
				mov bp, msg11	; bp = offset of string
				INT 0x10 ; bios video services

				mov ah, 0
				int 0x16
				cmp al, 121
				jne nextcheck
				jmp exit
				nextcheck:
					cmp al, 110
					jne exitkey

					mov ah, 0x13		; service 13 - print string
					mov al, 1			; subservice 01 – update cursor 
					mov bh, 0			; output on page 0
					mov bl, 00010111B	; normal attrib
					mov cx, 37	; length of string
					mov dx, 0x1314	; row 10 column 3
					push ds
					pop es				; es=ds segment of string
					mov bp, msg12	; bp = offset of string
					INT 0x10 ; bios video services
					mov ax, [fishposition]
					push ax
					call printfish
					jmp game

exit:
	mov dx, start ; end of resident portion
	add dx, 15 ; round up to next para
	mov cl, 4
	shr dx, cl ; number of paras
	call cls

    mov ax, 0x4c00 ; terminate and stay resident 
    int 0x21