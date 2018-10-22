;;README;;

; Ordem:
; (cria-mesa)
; (flop)
; (turn)
; (river) 
; (set-maos!)

; Funcoes (podem necessitar de passar outros argumentos para alem dos indicados):
; (JOGA) - comeca o jogo
; (cria-mesa) - comeca o jogo: da 1 carta a cada jogador. repete. Cria o vetor bank
; (flop) (turn) (river) - colocam cartas na mesa (3 + 1 + 1)
; (showdown) - faz os jogadores mostrar as cartas para o vencedor ser determinado. chama (winner)
; (set-maos!) - cria a lista maos, ordenada com as MAOS de cada jogador. necessario para analisar maos. Atualiza a lista maos.
; (jogador n) - DEVOLVE o JOGADOR n
; (ver-mao n) - DEVOLVE a MAO n
; (disp-jogadores) - DISPLAY dos JOGADORES
; (disp-mesa) - DISPLAY da MESA
; (disp-user) - (exemplo: As suas cartas: (As . Copas) (Valete . Espadas))
; (disp-bank) - mostra o dinheiro de cada jogador
; (disp-hands) - mostra o que cada jogador tem (straight, pair, etc) - nao usado pela funcao.
; (baralha!) - enche o BARALHO
; (winner) - devolve o vencedor
; (hand-vec) - devolve um vetor em que cada posicao contem a HAND de um jogador
; (hand mao) ;recebe uma MAO. devolve o correspondente (straight, pair, etc.). Exemplo: (hand (ver-mao 1))

; Definicoes:
; Uma CARTA e da seguinte forma (cons a b) sendo a um NUMERO com o valor da carta, b um SIMBOLO com o naipe.
; O BARALHO contem inicialmente 52 CARTAS, do qual e tirada uma aleatoriamente sempre que necessario
; Um JOGADOR e constituido pelas 2 CARTAS que este recebeu no inicio
; A MESA e constituida por um simbolo identificador 'Mesa e pelas 5 CARTAS que sao visiveis por todos
; Uma MAO e constituida por 7 CARTAS (2 do jogador + 5 da mesa)
; Uma HAND e uma lista que representa o melhor conjunto de 5 cartas do jogador (straight, par, etc.)
; Um DISPLAY mostra no ecra as cartas em forma de simbolo (forma normal)
; Um DEVOLVE mostra no ecra as cartas em forma de numero (Ex: Valete e convertido para 11)


(define espera 
  (lambda (t-ms)
    (let ((limite (+ (current-inexact-milliseconds) t-ms)))
      (letrec ((ciclo
                (lambda ()
                  (if (< (current-inexact-milliseconds) limite)
                      (ciclo)))))
        (ciclo)))))

;;;;;;;;;;;
;; JOGAR ;;
;;;;;;;;;;;

(define JOGA
  (lambda ()
    (display ".------..------..------..------..------.\n")
    (display "|P.--. ||O.--. ||K.--. ||E.--. ||R.--. |\n")
    (display "| :/\\: || :/\\: || :/\\: || (\\/) || :()  |\n")
    (display "| (__) || :\\/: || :\\/: || :\\/: || ()() |\n")
    (display "| '--'P|| '--'O|| '--'K|| '--'E|| '--'R|\n")
    (display "`------'`------'`------'`------'`------'\n")
    (display "Numero de jogadores (Max 8): ")
    (let ((num-j (read))) ;pede um numero de jogadores            
      (if (or (not (integer? num-j)) (> num-j 8) (< num-j 2)) ;caso nao seja introduzido um num valido
          (begin
            (display "Tente outra vez.\n")
            (JOGA))
          (begin
            (set! jogadores (make-vector num-j))
            (set! bank (make-vector num-j 10000))
            (set! bets (make-vector num-j))
            (display "------------------------------\n")
            (comeca-ronda 100 50 (random num-j)))))))

(define comeca-ronda ;utilizador e o jogador1
  (lambda (bigB smallB first) ;first=primeiro a jogar (indice). a esquerda do big blind  (bigblind +1). 0=jogador 1, 1=jogador 2...
    (set! first-to-act first) ;guarda o indice do 1o jogador.
    (clear-mesa)
    (cria-mesa)
    (display "Small Blind: ") (display smallB) (display "€ | Big Blind: ") (display bigB) (display "€\n")
    (display "Distribuindo cartas...") (espera 500)
    (display "\n------------------------------\n")
    (set-maos!)
    (set! bigBlind bigB)
    (set! smallBlind smallB)
    (set! temp (min smallBlind (vector-ref bank (modulo (sub1 first) (vector-length jogadores))))) ;temp(2)=small(big) blind, a nao ser que o jogador nao tenha
    (set! temp2 (min bigBlind (vector-ref bank (modulo (- first 2) (vector-length jogadores))))) ;dinheiro suficiente (se nao, fica o dinheiro que tem).   
    (pay-blind-small (modulo (- first 2) (vector-length jogadores))) ;mod para de 0 passar para 6, ou de 1 para 7. (caso 8 jogadores, claro)
    (pay-blind-big (modulo (sub1 first) (vector-length jogadores))) ;mod para de 0 passar para 7. NAO DEVERAO ALTERAR AS VARIAVEIS TEMP!
    (betting (+ temp temp2) first)))

(define betting ;bets
  (lambda (pot first) ;First=Indice
    (letrec ((cpubet (ceiling (* 0.01 (add1 (random 10)) (vector-ref bank first)))) ;aposta entre 1% a 10% do que tem
             (escolher-call-raise-fold 
              (lambda (input pot bet)
                (cond ((string-ci=? input "Call") (if (>= (left-to-bet 0 bet) (vector-ref bank 0)) ;se o jogador nao tiver dinheiro
                                                      (begin ;all in
                                                        (all-in 0) ;guarda em temp o dinheiro gasto.
                                                        (aux 1 (+ pot temp) bet)) 
                                                      (begin ;call
                                                        (call 0 bet)
                                                        (aux 1 (+ pot temp) bet))))
                      ((string-ci=? input "Raise") (display "------------------------------\n")
                                                   (if (> (vector-ref bank 0) (left-to-bet 0 (ceiling (* 1.3 bet)))) ;se tiver suficiente no banco. RAISE=(Ceiling (bet * 1.3))
                                                       (begin  ;raise
                                                         (raise 0 bet)
                                                         (aux 1 (+ pot temp) (ceiling (* 1.3 bet)))) ;atualiza-se o pot (soma-se temp). e a bet
                                                       (begin ;all-in.
                                                         (all-in 0)
                                                         (aux 1 (+ pot temp) (max bet (vector-ref bets 0)))))) ;max usa-se caso o jogador de raise sem dinheiro para call.
                      ((string-ci=? input "All-in") (all-in 0)
                                                    (aux 1 (+ pot temp) (max bet (vector-ref bets 0)))) ;max usa-se porque (ver acima)
                      ((string-ci=? input "Fold") (fold 1)
                                                  (aux 1 pot bet)) 
                      (else (display "\nErro: Introduza de novo.\n")
                            (mensagem-escolher-call-raise-fold pot bet) ;UI
                            (escolher-call-raise-fold (symbol->string (read)) pot bet)))))
             (escolher-bet-check ;i=0, pois apenas o jogador tem de escolher.
              (lambda (input pot) 
                (cond ((string-ci=? input "Bet") (display "------------------------------\n")
                                                 (display "Quanto deseja apostar? (Banco: ")
                                                 (display (vector-ref bank 0))
                                                 (display "€)\n")
                                                 (display "Aposta: ")
                                                 (let ((playerbet (read)))
                                                   (if (or (not (number? playerbet)) (not (integer? playerbet)) (<= playerbet 0) (> playerbet (vector-ref bank 0))) ;a bet tem de ser um inteiro entre 1 e o dinheiro que tem.
                                                       (escolher-bet-check "badstring" pot)
                                                       (if (= playerbet (vector-ref bank 0)) ;se apostar tudo:
                                                           (begin ;All in.
                                                             (all-in 0)
                                                             (aux 1 (+ pot temp) temp))
                                                           (begin ;bet
                                                             (do-bet 0 playerbet)
                                                             (aux 1 (+ pot playerbet) playerbet))))))
                      ((string-ci=? input "Check") (display "------------------------------\n")
                                                   (check 0)
                                                   (aux 1 pot -1))
                      (else (display "\nErro: Introduza de novo.\n")
                            (mensagem-escolher-bet-check pot)
                            (escolher-bet-check (symbol->string (read)) pot)))))
             (aux ;verifica cada jogador que falta apostar
              (lambda (i pot bet) ;bet=aposta mais alta. i=indice do jogador atual.
                (cond ((= (num-jogadores) 1) (showdown pot)) ;Se o jogador estiver sozinho, acaba a ronda.
                      ((equal? (vector-ref jogadores i) 0) (aux (remainder (add1 i) (vector-length jogadores)) pot bet)) ;simplesmente ignora se o jogador tiver dado fold                      
                      ((= (vector-ref bets i) bet) (draw-cards pot first)) ;se todos tiverem dado call, passa para a proxima fase
                      ((= 0 (vector-ref bank i)) (espera 300) (display "Jogador ") (display (add1 i)) (display " passa. Nao tem mais dinheiro para apostar.\n")
                                                 (vector-set! bets i bet)
                                                 (aux (remainder (add1 i) (vector-length jogadores)) pot bet))
                      ; Utilizador a jogar
                      ((= i 0) (if (= bet -1) ;se todos tiverem dado check ate agora
                                   (begin
                                     (mensagem-escolher-bet-check pot)
                                     (escolher-bet-check (symbol->string (read)) pot)) ;o jogador tem de escolher se da bet ou check
                                   (begin
                                     (mensagem-escolher-call-raise-fold pot bet)
                                     (escolher-call-raise-fold (symbol->string (read)) pot bet)))) ;o jogador escolhe se da call ou raise ou fold 
                      ; CPU a jogar
                      (else (espera (+ 200 (random 800)))
                            (if (= bet -1) ;se todos tiverem dado check ate agora.
                                (let ((cpubet2 (ceiling (* 0.01 (add1 (random 10)) (vector-ref bank i))))) ;entre 1 e 10% do dinheiro
                                  ;Tem de passar ai-fold 2 vezes.
                                  (cond ((or (ai-fold? i pot bet) (ai-fold? i pot bet)) (check i) ;check. conta aposta como 0.
                                                                                        (aux (remainder (add1 i) (vector-length jogadores)) pot -1)) ;continua aux, soma a bet ao pot. cpubet2 = bet
                                        (else (do-bet i cpubet2) ;bet
                                              (aux (remainder (add1 i) (vector-length jogadores)) (+ pot cpubet2) cpubet2))))
                                (cond ((ai-fold? i pot bet) (fold (add1 i)) ;fold
                                                            (aux (remainder (add1 i) (vector-length jogadores)) pot bet))
                                      ((or (ai-fold? i pot bet) (ai-fold? i pot bet) (ai-fold? i pot bet) ;passando ai-fold? mais 6 vezes, da raise.
                                           (ai-fold? i pot bet) (ai-fold? i pot bet) (ai-fold? i pot bet)) ;A ideia era fazer uma AI que soubesse quando dar fold e quando dar raise, por isso aproveita-se o mesmo procedimento para os dois.
                                       (if (> (vector-ref bank i) (left-to-bet i bet)) ;se tiver mais dinheiro do que a bet
                                           (begin  ;call
                                             (call i bet)
                                             (aux (remainder (add1 i) (vector-length jogadores)) (+ pot temp) bet)) ;atualiza-se o pot (soma-se temp). e a bet
                                           (begin ;all-in. 
                                             (all-in i)
                                             (aux (remainder (add1 i) (vector-length jogadores)) (+ pot temp) bet))))
                                      (else (if (> (vector-ref bank i) (left-to-bet i (ceiling (* 1.3 bet)))) ;se tiver suficiente no banco
                                                (begin  ;raise
                                                  (raise i bet)
                                                  (aux (remainder (add1 i) (vector-length jogadores)) (+ pot temp) (ceiling (* 1.3 bet)))) ;atualiza-se o pot (soma-se temp). e a bet
                                                (begin ;all-in. pode acontecer mesmo que nao haja dinheiro para call.
                                                  (all-in i)
                                                  (aux (remainder (add1 i) (vector-length jogadores)) (+ pot temp) (max bet (vector-ref bets i))))))))))))) ;Fim AUX
      (espera 400)
      (cond ((<= (has-money) 1) (draw-cards pot first)) ;se um ou menos tiver dinheiro, passa para a proxima fase
            ((equal? (pre-flop-turn-river?) 'pre) (aux first pot (max temp temp2))) ;caso pre-flop, bet = maior aposta entre o small blind e o big blind (em principio sera big blind)
            ((zero? (vector-ref bank first)) (display "Jogador ") (display (add1 first)) (display " passa. Nao tem mais dinheiro para apostar.\n")
                                             (vector-set! bets first -1) ;check
                                             (aux (remainder (add1 first) (vector-length jogadores)) pot -1)) ;Se nao tiver dinheiro para apostar, e como se tivesse feito um check. Nao da display
            ((= first 0) (mensagem-escolher-bet-check pot)
                         (escolher-bet-check (symbol->string (read)) pot))
            ((ai-fold? first pot 0) (check first) ;Check
                                    (aux (remainder (add1 first) (vector-length jogadores)) pot -1))
            (else (do-bet first cpubet) ;bet
                  (aux (remainder (add1 first) (vector-length jogadores)) (+ pot cpubet) cpubet)))))) ;Fim (Betting)

(define showdown
  (lambda (pot)
    (espera 400) (display "------------------------------\n") (espera 400)
    (disp-jogadores)
    (set-maos!) (espera 250)
    (disp-mesa) (espera 250)
    (winner pot)))

(define recomeca-ronda
  (lambda (first) ;passa o primeiro da PROXIMA ronda
    (disp-bank)
    (set! bets (make-vector (vector-length jogadores)))
    (set! smallBlind (ceiling (* 1.1 smallBlind))) ;aumenta as blinds
    (set! bigBlind (* smallBlind 2))
    (comeca-ronda bigBlind smallBlind first)))


(define fold ;da fold ao jogador n. substitui as cartas do jogador por 0. As funcoes sao forcadas a ignorar o 0. refresh a maos
  (lambda (n)    
    (display "Jogador ") (display n) (display " desistiu\n")
    (if (= n 1)        
        (begin
          (display "\n------------------------------\n")
          (display "Observando o jogo...\n") (espera 300)))
    (vector-set! jogadores (sub1 n) 0)
    (set-maos!)))

(define bets (vector)) ;contem as apostas de cada jogador.
(define first-to-act -1) ;vai guardar o primeiro jogador.
(define bigBlind 0)
(define smallBlind 0)

;;;;;;;;;;;
;; BANCO ;;
;;;;;;;;;;;

(define pay-blind-big ;paga big-blind do jogador i.
  (lambda (i)
    (display "Jogador ") (display (add1 i)) (display " paga Big Blind (") (display (min bigBlind (vector-ref bank i))) (display "€)\n") ;min caso o jogador nao tenha dinheiro suficiente.
    (vector-set! bets i bigBlind)
    (vector-set! bank i (max 0 (- (vector-ref bank i) bigBlind))))) ;nao pode ficar a menos de 0.

(define pay-blind-small ;paga small-blind do jogador i.
  (lambda (i)
    (display "Jogador ") (display (add1 i)) (display " paga Small Blind (") (display (min smallBlind (vector-ref bank i))) (display "€)\n") ;min caso o jogador nao tenha dinheiro suficiente.
    (vector-set! bets i smallBlind)
    (vector-set! bank i (max 0 (- (vector-ref bank i) smallBlind)))))

(define check
  (lambda (i)
    (vector-set! bets i -1) ;check.
    (display "Jogador ") (display (add1 i)) (display " passa\n")))

(define all-in  ;recebe o indice do jogador. aposta tudo o que o jogador tem.
  (lambda (i)
    (set! temp (vector-ref bank i)) ;guarda o dinheiro que resta em temp.
    (vector-set! bets i (+ (max 0 (vector-ref bets i)) temp)) ;atualiza dinheiro apostado
    (vector-set! bank i 0) ;tira o dinheiro do banco
    (display "Jogador ") (display (add1 i)) (display " aposta tudo. (") (display temp) (display "€)\n")))

(define call ;recebe o indice do jogador e a aposta. tira o que falta apostar ao jogador do banco dele. ALTERA a aposta do jogador para bet.
  (lambda (i bet)
    (set! temp (left-to-bet i bet)) ;temp=quantidade que se aumentou na bet.(max 0 ..) caso bet seja -1.
    (vector-set! bank i (- (vector-ref bank i) temp)) ;tira-se temp ao banco
    (vector-set! bets i bet) ;atualiza-se a bet do jogador
    (display "Jogador ") (display (add1 i)) (display " iguala com ") (display bet) (display "€\n")))

(define do-bet ;recebe o indice do jogador e a aposta. tira o dinheiro da aposta ao jogador. ALTERA a aposta do jogador para aposta.
  (lambda (i bet)
    (vector-set! bets i bet)
    (display "Jogador ") (display (add1 i)) (display " aposta ") (display bet) (display "€\n")
    (vector-set! bank i (- (vector-ref bank i) bet))))

(define raise ;aumenta a aposta em 30%
  (lambda (i bet)
    (set! temp (left-to-bet i (ceiling (* 1.3 bet)))) ;temp=quantidade que se aumentou na bet DO JOGADOR.
    (vector-set! bank i (- (vector-ref bank i) temp)) ;tira-se temp ao banco
    (vector-set! bets i (ceiling (* 1.3 bet))) ;atualiza-se a bet do jogador
    (display "Jogador ") (display (add1 i)) (display " sobe para ") (display (ceiling (* 1.3 bet))) (display "€\n")))

(define left-to-bet ;devolve a aposta menos o que ja foi apostado
  (lambda (i bet)
    (- bet (max 0 (vector-ref bets i))))) ;max caso o jogador tenha apostado -1-check

(define split-pot! ;divide pot pelos vencedores
  (lambda (vencedores pot) ;(1 3 6) 30. fatia=10
    (letrec ((fatia (/ pot (length vencedores)))
             (aux
              (lambda (lista)
                (if (null? lista)
                    (display "")
                    (begin
                      (vector-set! bank (sub1 (car lista)) (+ fatia (vector-ref bank (sub1 (car lista)))))
                      (aux (cdr lista)))))))
      (aux vencedores))))

(define clear-bankrupt ;Acaba o jogo se o jogador nao tiver dinheiro. Se tiver, apaga os jogadores que nao tiverem dinheiro para continuar. Recomeca ronda, passa first soma-lhe 1(mod num-jogadores)
  (lambda ()
    (letrec ((aux1 ;poe a 1 os jogadores a apagar (0 sao os folded)
              (lambda (i)
                (cond ((= i (vector-length bank)) (set! bank (list->vector (apaga-zeros (vector->list bank)))) ;apaga os zeros de bank
                                                  (set! jogadores (list->vector (apaga-uns (vector->list bank))))) ;apaga os uns de jogadores
                      ((= (vector-ref bank i) 0) (vector-set! jogadores i 1)
                                                 (display "O jogador ") (display (add1 i)) (display " perdeu.\n")
                                                 (aux1 (add1 i)))
                      (else (aux1 (add1 i)))))))
      (if (= 0 (vector-ref bank 0)) ;se o jogador nao tem dinheiro
          (begin
            (disp-bank)
            (display "Nao tem mais dinheiro. Perdeu! Ficou em ")
            (display (vector-length jogadores))
            (display "o lugar.\n"))
          (begin (aux1 0)
                 (if (= (vector-length jogadores) 1) ;se o jogador e o unico restante
                     (display "\nOs seus adversarios nao tem mais dinheiro. Ganhou!")
                     (begin
                       (mensagem-recomecando-ronda)
                       (recomeca-ronda (remainder (add1 first-to-act) (vector-length jogadores)))))))))) ;os jogadores que perderam ja foram apagados!! (remainder para que de 7 passe para 0 (caso sejam 8 jogadores). first-to-act pois first ja foi alterado

(define disp-bank
  (lambda ()
    (letrec ((aux
              (lambda (i)
                (if (= i (vector-length bank))
                    (display "")
                    (begin
                      (espera 150) (display "Jogador ") (display (add1 i)) (display ": ")
                      (display (vector-ref bank i))
                      (display "€\n")
                      (aux (add1 i)))))))
      (display "\n------------------------------\n")
      (display "Banco\n")
      (aux 0)
      (display "------------------------------\n"))))

(define has-money ;numero de pessoas que tem dinheiro.
  (lambda ()
    (letrec ((aux
              (lambda (i ctr)
                (cond ((= i (vector-length bank))  ctr)
                      ((or (= 0 (vector-ref bank i)) (equal? 0 (vector-ref jogadores i))) (aux (add1 i) ctr))
                      (else (aux (add1 i) (add1 ctr)))))))
      (aux 0 0))))

(define hand-value ;recebe uma hand. devolve um valor aproximado (par de ases =
  (lambda (handx)
    (case (car handx)
      ((0 1 2 3 5 6 7) (+ (* 10 (car handx)) (floor (* 10 (/ (cadr handx) 14.1))))) ;a carta maxima e 14, nao se quer igualar um As a um par de 2. o objetivo e manter as unidades a representar o criterio de desempate.
      ((4 8 9) (+ (* 10 (car handx)) (floor (* 10 (/ (cdr handx) 14.1))))))))

(define bank (vector))

;;;;;;;;
;; AI ;;
;;;;;;;;

(define log1.04 ;logaritmo base 1.04. Procedimento Auxiliar.
  (lambda (n)
    (/ (log n) (log 1.04))))

(define hand-value->chance-to-fold ;Auxiliar a inteligencia artificial!. Objetivo de garantir baixas probabilidades ate aos pares, medias entre pares e trios, altas a partir de sequencia. 
  (lambda (value to-bet pot money) ;recebe o valor da mao, o que falta apostar e o dinheiro que tem.
    (max 0 (- 100 (max 0 (ceiling (- (log1.04 (max 1 value)) (/ to-bet (+ (max 0.0001 money) (/ pot (vector-length jogadores) 0.02)))))))))) ;Tem em conta tambem a percentagem do dinheiro que ira perder. 0.02-fator de importancia do dinheiro. Quando maior o fator, menos o CPU se preocupa com o dinheiro em jogo, preocupa-se mais com a qualidade do seu jogo.

(define lottery-win? ;recebe uma probabilidade, devolve #t caso vencedor.
  (lambda (odds) ;percentagem.
    (let ((num (add1 (random 100)))) ;num de 1 a 100
      (<= num odds))))

(define ai-fold? ;No fundo e aleatorio!
  (lambda (i pot bet) ;recebe indice do jogador e a aposta
    (let ((odd (hand-value->chance-to-fold (hand-value (hand (ver-mao (add1 i)))) (left-to-bet i bet) pot (vector-ref bank i))))
      (lottery-win? odd))))



;;;;;;;;;;;;;;;;
;; Dar cartas ;;
;;;;;;;;;;;;;;;;

(define cria-mesa 
  (lambda ()
    (letrec ((da-cartas1 ;da uma carta a cada jogador
              (lambda (i)
                (if (not (= i (vector-length jogadores)))
                    (begin
                      (vector-set! jogadores i (tira-carta)) ;note-se que tira-carta tambem devolve a carta.
                      (da-cartas1 (add1 i))))))
             (da-cartas2 ;da outra carta a cada jogador
              (lambda (i)
                (if (not (= i (vector-length jogadores)))
                    (begin
                      (vector-set! jogadores i (append (list (vector-ref jogadores i)) (list (tira-carta)))) ;
                      (da-cartas2 (add1 i)))))))
      (baralha!)
      (da-cartas1 0)
      (da-cartas2 0))))


(define elimina-carta! ;remove 'carta' do vetor baralho.
  (lambda (carta)
    (letrec ((nbaralho (make-vector (sub1 (vector-length baralho))))
             (aux
              (lambda (i)
                (cond ((= i (vector-length baralho)) #f)
                      ((equal? carta (vector-ref baralho i)) (aux2 i))
                      (else (vector-set! nbaralho i (vector-ref baralho i))
                            (aux (add1 i))))))
             (aux2
              (lambda (i)
                (cond ((= i (sub1 (vector-length baralho))) (vector-set! baralho i 0))
                      (else (vector-set! nbaralho i (vector-ref baralho (add1 i)))
                            (aux2 (add1 i)))))))
      (aux 0)
      (set! baralho nbaralho))))

(define tira-carta ;escolhe uma carta aleatoria. devolve a carta e remove-a do baralho
  (lambda ()
    (if (positive? (vector-length baralho))
        (begin
          (let* ((num (random (vector-length baralho))) ;escolhe um indice a sorte
                 (carta (vector-ref baralho num))) ;ve a carta correspondente
            (elimina-carta! carta)
            carta))
        (display "Acabaram-se as cartas!"))))

(define clear-mesa
  (lambda ()
    (set! mesa (list 'Mesa: ))))

(define flop ;adiciona cartas a mesa e chama betting. Limpa as bets
  (lambda (pot first)
    (append! mesa (list (tira-carta)))
    (append! mesa (list (tira-carta)))
    (append! mesa (list (tira-carta)))
    (display "------------------------------\n")
    (display "FLOP\n")
    (set! bets (make-vector (vector-length jogadores)))
    (set-maos!)
    (betting pot (temp-first first)))) ;altera temporariamente o primeiro a agir.  (ve-se o antigo ja tinha dado fold)


(define turn  ;adiciona cartas a mesa e chama betting. Limpa as bets
  (lambda (pot first)
    (append! mesa (list (tira-carta)))    
    (display "------------------------------\n")
    (display "TURN\n")
    (set! bets (make-vector (vector-length jogadores)))
    (set-maos!)
    (betting pot (temp-first first)))) ;altera temporariamente o primeiro a agir. (ve-se o antigo ja tinha dado fold)

(define river ;adiciona cartas a mesa e chama betting. Limpa as bets
  (lambda (pot first)
    (append! mesa (list (tira-carta)))
    (display "------------------------------\n")
    (display "RIVER\n")
    (set! bets (make-vector (vector-length jogadores)))
    (set-maos!)
    (betting pot (temp-first first)))) ;altera temporariamente o primeiro a agir.  (ve-se o antigo ja tinha dado fold)

(define draw-cards ;poe cartas na mesa/faz os jogadores mostrar as cartas dependendo da fase do ronda
  (lambda (pot first) ;passa estes dois argumentos
    (case (pre-flop-turn-river?)
      ((pre) (espera 500) (flop pot first))
      ((flop) (espera 500) (turn pot first))
      ((turn) (espera 500) (river pot first))
      ((river) (espera 500) (showdown pot)))))

;;;;;;;;;;;;;;;
;; Seletores ;;
;;;;;;;;;;;;;;;

(define num-carta
  (lambda (carta)
    (car carta)))

(define naipe
  (lambda (carta)
    (cdr carta)))


(define jogador ;devolve as cartas do jogador n
  (lambda (n)
    (vector-ref jogadores (sub1 n))))

(define num-jogadores ;devolve o numero de jogadores em jogo (nao conta os que desistiram)
  (lambda ()
    (letrec ((aux 
              (lambda (i ctr)
                (cond ((= i (vector-length jogadores)) ctr)
                      ((equal? (vector-ref jogadores i) 0) (aux (add1 i) ctr))
                      (else (aux (add1 i) (add1 ctr)))))))
      (aux 0 0))))

(define disp-carta ;mostra tambem as figuras
  (lambda (carta)
    (cond ((< (num-carta carta) 11) (display carta))
          (else (case (num-carta carta)
                  ((11) (display (cons 'Valete (naipe carta))))
                  ((12) (display (cons 'Dama (naipe carta))))
                  ((13) (display (cons 'Rei (naipe carta))))
                  ((14) (display (cons 'As (naipe carta)))))))))

(define disp-user ;mostra as cartas do utilizador, assume que tem 2.
  (lambda ()
    (display "\nAs suas cartas: ")
    (disp-carta (car (jogador 1)))
    (display " ")
    (disp-carta (cadr (jogador 1)))
    (display " | ")
    (num->hand (car (hand (ver-mao 1)))) ;mostra o que o jogador tem
    (newline)))

(define disp-jogadores ;display cartas dos jogadores. cada carta entre parentesis
  (lambda ()
    (letrec ((aux
              (lambda (i)
                (if (not (= i (vector-length jogadores)))
                    (begin
                      (espera 250)
                      (display "Jogador ")
                      (display (add1 i))
                      (if (= i 0)
                          (display " (Voce)"))
                      (display ": ")
                      (if (equal? (vector-ref jogadores i) 0)
                          (begin
                            (display "Desistiu\n")
                            (aux (add1 i)))
                          (begin
                            (disp-carta (car (vector-ref jogadores i)))
                            (display " ")
                            (disp-carta (cadr (vector-ref jogadores i)))
                            (display ": ")
                            (num->hand (car (hand (ver-mao (add1 i))))) ;mostra o que o jogador tem
                            (newline)
                            (aux (add1 i)))))))))
      (aux 0)
      (newline))))

(define disp-mesa ;o mesmo que disp-jogadores mas para a mesa
  (lambda ()
    (letrec ((aux
              (lambda (lista i)
                (if (not (null? lista))
                    (begin                      
                      (if (or (= i 4) (= i 5))
                          (display " | ")
                          (display " "))
                      (disp-carta (car lista))
                      (aux (cdr lista) (add1 i)))))))
      (display "Mesa:")
      (if (= (length mesa) 1)
          (display " Vazia")
          (aux (cdr mesa) 1))
      (newline))))

(define pre-flop-turn-river? ;diz se estao em pre-flop, flop, turn ou river.
  (lambda ()
    (case (sub1 (length mesa))
      ((0) 'pre)
      ((3) 'flop)
      ((4) 'turn)
      ((5) 'river))))

(define disp-hands ;mostra a hand de cada jogador
  (lambda ()
    (letrec ((aux
              (lambda (i)
                (if (not (= i (vector-length jogadores)))
                    (begin
                      (display "Jogador ")
                      (display (add1 i))
                      (display ": ")
                      (num->hand (car (hand (ver-mao (add1 i))))) ;add1 i = posicao.
                      (newline)
                      (aux (add1 i)))))))
      (aux 0))))

(define ver-mao ;devolve a mao de n. assume que n toma valores possiveis. MAO -> 7 cartas. necessita de set-maos!
  (lambda (n)
    (list-ref maos (sub1 n))))

(define tem-carta? ;Recebe mao e carta. Devolve boolean.
  (lambda (mao carta)
    (if (member carta mao)
        #t
        #f)))

(define tem-num? ;recebe mao e num. Ex: (tem-num? (ver-mao 1) 4) devolve #t se j1 tiver um quatro.
  (lambda (mao num)
    (if (assoc num mao)
        #t
        #f)))

;;;;;;;;;;
;; HAND ;;
;;;;;;;;;;       

(define hand ;recebe uma MAO de 7 cartas. devolve o correspondente (straight, pair, etc.). Exemplo: (hand (ver-mao 1))
  (lambda (mao)
    (clear-naipes)
    (ordena-naipes mao) ;Unica funcao que podera ALTERAR o vetor naip-ord
    (cond ((null? mao) (cons -1 0)) ;se o jogador tiver desistido, (-1 0)
          ((royal? mao) (cons 9 0)) ;nao ha royals melhores do que outros   
          ((straight-flush mao) (cons 8 (straight-flush mao))) ;melhor carta da sequencia
          ((four mao) (cons 7 (four mao))) ;num das 4 cartas + carta mais alta. 
          ((full mao) (cons 6 (full mao))) ;num do trio + num do par.
          ((flush) (cons 5 (flush))) ;5 melhores cartas do flush, da menor para a maior. (NAO tem parametros)
          ((straight mao) (cons 4 (straight mao))) ;melhor carta da sequencia (sendo uma sequencia composta por 5 cartas, se a sequencia for igual empata-se)
          ((three mao) (cons 3 (three mao))) ;num do trio + 2 cartas mais altas
          (else (count-pair mao))))) ;2 pares: (2. (par-maior par-menor maior-carta)) - cartas mais altas sem os pares
;1 par: (1. (par. 3cartas-mais-altas)) - cartas mais altas sem o par
;else, (0. 5 cartas mais altas)

(define royal? ;Verifica as 4 possibilidades de mao. 
  (lambda (mao)
    (or (and (tem-carta? mao (cons 14 'Espadas))
             (tem-carta? mao (cons 13 'Espadas))
             (tem-carta? mao (cons 12 'Espadas))
             (tem-carta? mao (cons 11 'Espadas))
             (tem-carta? mao (cons 10 'Espadas)))
        (and (tem-carta? mao (cons 14 'Ouros))
             (tem-carta? mao (cons 13 'Ouros))
             (tem-carta? mao (cons 12 'Ouros))
             (tem-carta? mao (cons 11 'Ouros))
             (tem-carta? mao (cons 10 'Ouros)))
        (and (tem-carta? mao (cons 14 'Copas))
             (tem-carta? mao (cons 13 'Copas))
             (tem-carta? mao (cons 12 'Copas))
             (tem-carta? mao (cons 11 'Copas))
             (tem-carta? mao (cons 10 'Copas)))
        (and (tem-carta? mao (cons 14 'Paus))
             (tem-carta? mao (cons 13 'Paus))
             (tem-carta? mao (cons 12 'Paus))
             (tem-carta? mao (cons 11 'Paus))
             (tem-carta? mao (cons 10 'Paus))))))

(define straight-flush ;Verifica a mao contida em naip-ord. ASSUME-SE QUE FOI CHAMADO POR (HAND)! (clear-naipes) + (ordena-naipes) tem de ser chamado primeiro.
  (lambda (mao)
    (letrec ((aux
              (lambda (i)
                (cond ((= i 4) #f) ;se nao houver flush
                      ((>= (length (vector-ref naip-ord i)) 5) (aux2 (vector-ref naip-ord i) 1 0)) ;a condicao verifica se existe um flush. Se sim, verifica se esse flush e uma sequencia
                      (else (aux (add1 i))))))
             (aux2 ;semelhante a straight, mas bastante mais simples pois 1. cartas do mesmo naipe -> nao haverao numeros repetidos
              (lambda (lista ctr high) ; 2. trabalha-se com listas de inteiros, nao de pares.
                (cond ((null? (cdr lista)) (cond ((>= ctr 5) high)
                                                 ((and (member 14 mao) (member 2 mao) (member 3 mao) (member 4 mao) (member 5 mao)) 5) ;equivalente a ace-to-five?
                                                 (else #f)))
                      ((= (- (cadr lista) (car lista)) 1) (aux2 (cdr lista) (add1 ctr) (cadr lista)))
                      ((>= ctr 5) high)
                      (else (aux2 (cdr lista) 1 0))))))
      (aux 0))))

(define four 
  (lambda (mao)
    (letrec ((mao-revrt (reverse mao)); Analisar a lista REVERTIDA (maior para menor).
             (aux
              (lambda (lista ctr num)
                (cond ((= ctr 4) (cons num (maior-cart-exceto mao-revrt num))) ;se foram encontradas 4 iguais, devolve a carta + carta mais alta (sem ser a do poker)
                      ((null? (cdr lista)) #f) ;se nao, devolve falso
                      ((= (caar lista) (caadr lista)) (aux (cdr lista) (add1 ctr) (caadr lista))) ;se a carta atual e igual a seguinte, soma-se um ao contador.
                      (else (aux (cdr lista) 1 (caadr lista)))))) ;se nao, da-se reset ao contador
             (maior-cart-exceto ;recebe uma mao revertida (maior para menor). devolve a maior carta que nao seja igual a a.
              (lambda (mao a)
                (letrec ((aux2
                          (lambda (lista)
                            (cond ((null? lista) 0) ;nunca devera acontecer
                                  ((= (num-carta (car lista)) a) (aux2 (cdr lista)))
                                  (else (num-carta (car lista)))))))
                  (aux2 mao)))))
      (aux mao-revrt 1 (caar mao-revrt)))))

(define full ;devolve (num-trio . num-par)
  (lambda (mao)
    (letrec ((mao-revrt (reverse mao)); Analisar a lista REVERTIDA (maior para menor).
             (proc-par-trio ;procura, do maior para o menor, o primeiro par ou trio que apareca
              (lambda (lista ctr num)
                (cond ((null? (cdr lista)) #f) ;se estiver no fim, mesmo que haja um trio ou um par, nao sera possivel existir um full-house
                      ((= (caar lista) (caadr lista)) (proc-par-trio (cdr lista) (add1 ctr) (caadr lista)))
                      ((= ctr 3) (proc-par (cdr lista) num)) ;se encontrar um trio, vai procurar o par
                      ((= ctr 2) (proc-trio (cdr lista) 1 (caadr lista) num)) ;se encontrar um par, vai procurar o trio
                      (else (proc-par-trio (cdr lista) 1 (caadr lista)))))) ;reset ao contador se nao encontrar
             (proc-par ;procura o maior par, sendo que o trio ja foi encontrado. continua com a lista fornecida.
              (lambda (lista num-trio) ;num-trio: ja encontrado
                (cond ((null? (cdr lista)) #f)
                      ((= (caar lista) (caadr lista)) (cons num-trio (caar lista))) ;se o par for encontrado. devolve (num-trio . num-par)
                      (else (proc-par (cdr lista) num-trio)))))
             (proc-trio ;procura o maior trio, sendo que o par ja foi encontrado. continua com a lista fornecida.
              (lambda (lista ctr num num-par) ;num-par: ja encontrado. num: carta atual
                (cond ((= ctr 3) (cons num num-par))  ;encontrado o trio, devolve (num-trio . num-par)
                      ((null? (cdr lista)) #f)
                      ((= (caar lista) (caadr lista)) (proc-trio (cdr lista) (add1 ctr) (caadr lista) num-par))
                      (else (proc-trio (cdr lista) 1 (caadr lista) num-par))))))              
      (proc-par-trio mao-revrt 1 (caar mao-revrt)))))

(define flush ;Verifica a mao contida em naip-ord. ASSUME-SE QUE FOI CHAMADO POR (HAND)! (clear-naipes) + (ordena-naipes) tem de ser chamado primeiro.
  (lambda ()
    (letrec ((aux
              (lambda (i)
                (cond ((= i 4) #f)
                      ((= (length (vector-ref naip-ord i)) 5) (vector-ref naip-ord i))
                      ((= (length (vector-ref naip-ord i)) 6) (cdr (vector-ref naip-ord i)))
                      ((= (length (vector-ref naip-ord i)) 7) (cddr (vector-ref naip-ord i)))
                      (else (aux (add1 i)))))))
      (aux 0))))


(define straight ;devolve a carta mais alta DA SEQUENCIA caso esta exista na MAO.
  (lambda (mao)
    (letrec ((aux 
              (lambda (lista ctr high) ;high = highcard. contam-se 5 cartas seguidas.
                (cond ((null? (cdr lista)) (cond ((>= ctr 5) high)
                                                 ((ace-to-five?) 5)
                                                 (else #f)))
                      ((= (- (num-carta (cadr lista)) (num-carta (car lista))) 0) (aux (cdr lista) ctr high)) ;se a prox carta for igual, nao se aumenta o contador.
                      ((= (- (num-carta (cadr lista)) (num-carta (car lista))) 1) (aux (cdr lista) (add1 ctr) (num-carta (cadr lista)))) ;se a prox carta for exatamente uma acima, soma-se um ao contador. high=prox carta
                      ((>= ctr 5) high) ;se a sequencia acabou, mas ja se contaram 5 cartas, devolve-se high.
                      (else (aux (cdr lista) 1 0))))) ;se a sequencia acabou, mas nao se encontraram 5 cartas, da-se reset ao contador e ao high.
             (ace-to-five?  ;verifica se a sequencia as-cinco existe
              (lambda ()
                (and (tem-num? mao 14)
                     (tem-num? mao 2)
                     (tem-num? mao 3)
                     (tem-num? mao 4)
                     (tem-num? mao 5)))))
      (aux mao 1 0))))

(define three
  (lambda (mao)
    (letrec ((mao-revrt (reverse mao))
             (aux
              (lambda (lista ctr)
                (cond ((= ctr 3) (cons (num-carta (car lista)) (2maior-cart-exceto mao-revrt (num-carta (car lista))))) ;se o trio for encontrado. devolve (num-carta . (carta-maior 2a-carta-maior)), exceto as cartas do trio
                      ((null? (cdr lista)) #f)
                      ((= (num-carta (car lista)) (num-carta (cadr lista))) (aux (cdr lista) (add1 ctr))) ;add1 ao contador se a carta seguinte for igual.
                      (else (aux (cdr lista) 1)))))
             (2maior-cart-exceto ;recebe uma mao revertida (maior para menor). devolve as 2 maiores cartas que nao sejam iguais a a
              (lambda (mao a) ;a e um numero
                (letrec ((aux2
                          (lambda (lista nlista) ;nlista = lista das 2 cartas
                            (cond ((= (length nlista) 2) nlista)
                                  ((null? lista) 0) ;Nunca devera acontecer
                                  ((= (num-carta (car lista)) a) (aux2 (cdr lista) nlista))
                                  (else (aux2 (cdr lista) (append nlista (list (num-carta (car lista))))))))))
                  (aux2 mao '())))))
      (aux mao-revrt 1))))

(define count-pair ;Assume que nao existe trio!!!!! Conta o num de pares (0, 1 ou 2)
  (lambda (mao)
    (letrec ((mao-revrt (reverse mao))
             (par1 ;procura o maior par. #f se nao houver nenhuum par
              (lambda (lista)
                (cond ((null? (cdr lista)) (cons 0 (high-cards mao))) ;nao havendo par devolve as 5 cartas mais altas (Maior-Menor)
                      ((= (num-carta (car lista)) (num-carta (cadr lista))) (par2 (cdr lista) (num-carta (cadr lista))))
                      (else (par1 (cdr lista))))))
             (par2 ;procura o 2o maior par.
              (lambda (lista par1) ;par1 = num do par maior
                (cond ((null? (cdr lista)) (cons 1 (cons par1 (3maior-cart-exceto mao-revrt par1)))) ;se nao for encontrado devolve: (1 . (par1 . 3maiores-cartas))
                      ((= (num-carta (car lista)) (num-carta (cadr lista))) (cons 2 (list par1 (num-carta (cadr lista)) (maior-cart-exceto mao-revrt par1 (num-carta (cadr lista)))))) ;(num-carta (cadr lista)) neste caso = par2.   devolve: (2 . (par1 par2 maior-carta))
                      (else (par2 (cdr lista) par1)))))
             (3maior-cart-exceto ;recebe uma mao revertida (maior para menor). devolve as 3 maiores cartas que nao sejam iguais a a
              (lambda (mao a) ;a e um numero
                (letrec ((aux
                          (lambda (lista nlista) ;nlista = lista das 3 cartas
                            (cond ((= (length nlista) 3) nlista)
                                  ((null? lista) 0) ;Nunca devera acontecer
                                  ((= (num-carta (car lista)) a) (aux (cdr lista) nlista))
                                  (else (aux (cdr lista) (append nlista (list (num-carta (car lista))))))))))
                  (aux mao '()))))
             (maior-cart-exceto ;recebe uma mao revertida (maior para menor). devolve a maior carta que nao seja igual a a ou b
              (lambda (mao a b)
                (letrec ((aux
                          (lambda (lista)
                            (cond ((null? lista) 0) ;nunca devera acontecer
                                  ((or (= (num-carta (car lista)) a) (= (num-carta (car lista)) b)) (aux (cdr lista)))
                                  (else (num-carta (car lista)))))))
                  (aux mao)))))
      (par1 mao-revrt))))


(define high-cards ;devolve 5 cartas mais altas.
  (lambda (mao)
    (let ((mao-revrt (reverse mao)))
      (if (= (length mao) 2)
          (list (num-carta (car mao-revrt))
                (num-carta (cadr mao-revrt)))
          (list (num-carta (car mao-revrt))
                (num-carta (cadr mao-revrt))
                (num-carta (caddr mao-revrt))
                (num-carta (cadddr mao-revrt))
                (num-carta (cadddr (cdr mao-revrt))))))))

(define num->hand ;recebe um num e devolve o respetivo hand
  (lambda (n)
    (case n
      ((0) (display "High Card"))
      ((1) (display "One Pair"))
      ((2) (display "Two Pairs"))
      ((3) (display "Three of a Kind"))
      ((4) (display "Straight"))
      ((5) (display "Flush"))
      ((6) (display "Full House"))
      ((7) (display "Four of a Kind"))
      ((8) (display "Straight Flush"))
      ((9) (display "Royal Straight Flush")))))


(define hand-vec ;cria um vetor das hands dos jogadores
  (lambda ()
    (letrec ((new-vec (make-vector (vector-length jogadores)))
             (aux
              (lambda (i)
                (if (= i (vector-length new-vec))
                    new-vec
                    (begin
                      (vector-set! new-vec i (hand (ver-mao (add1 i))))
                      (aux (add1 i)))))))
      (aux 0))))

(define winner ;chama (desempata) sempre, mesmo que so haja um jogador vencedor.
  (lambda (pot)
    (letrec ((hands (hand-vec)) ;vetor com a hand de cada jogador
             (aux ;ve qual e a melhor hand
              (lambda (i maior nlista) ;maior = num da melhor hand. nlista = lista com os jogadores vencedores. i = posicao na lista, (add1) sempre que se fizer (cdr lista)
                (cond ((= i (add1 (vector-length hands))) (desempata nlista hands pot)) ;add1 pois i comeca a 1
                      ((> (car (vector-ref hands (sub1 i))) maior) (aux (add1 i) (car (vector-ref hands (sub1 i))) (list i))) ;ao encontrar uma mao melhor, nlista apenas tera um elemento.
                      ((= (car (vector-ref hands (sub1 i))) maior) (aux (add1 i) maior (append nlista (list i)))) ;ao encontrar um igual, junta-o aos melhores jogadores
                      (else (aux (add1 i) maior nlista))))))
      (aux 1 0 '()))))

;;;;;;;;;;;;;;;
;; Desempata ;;
;;;;;;;;;;;;;;;

(define desempata ;recebe uma lista de numeros (jogadores). chama disp-vencedores. 
  (lambda (jogadores hands pot) ;hands: vetor de hands. jogadores: numero dos jogadores a serem desempatados
    (letrec ((hand-list (vector->list-exceto hands jogadores)) ;hand-list: hands dos jogadores a serem desempatados. 
             (aux
              (lambda (lista lista-jog vencedores) ;lista: lista de hands. lista-jog: lista de numeros.  vencedores: lista de vencedores
                (cond ((null? (cdr lista)) (disp-vencedores vencedores (caar lista) pot)) ;tem de haver pelo menos um vencedor. lista = lista com a hand vencedora. (caar lista) = numero da hand vencedora. VENCEDORES = numeros dos vencedores
                      ((equal? (versus (car lista) (cadr lista)) 1) (set-cdr! lista (cddr lista)) ;apaga-se o perdedor da lista (2o elem)
                                                                    (set-cdr! lista-jog (cddr lista-jog)) ;desta tambem
                                                                    (aux lista lista-jog vencedores)) ;vencedores mantem-se
                      ((equal? (versus (car lista) (cadr lista)) 2) (set-car! lista (cadr lista));apaga-se o perdedor da lista (1o elem)
                                                                    (set-cdr! lista (cddr lista))
                                                                    (set-car! lista-jog (cadr lista-jog)) ;desta tambem
                                                                    (set-cdr! lista-jog (cddr lista-jog))
                                                                    (aux lista lista-jog (list (car lista-jog)))) ;atualiza vencedores. (car lista-jog) - vencedor (nao e cadr pois ja se apagou o perdedor)
                      ((equal? (versus (car lista) (cadr lista)) 'tie) (aux (cdr lista) (cdr lista-jog) (append vencedores (list (cadr lista-jog))))))))) ;como houve empate, pode usar-se o elemento seguinte como termo de comparacao. atualiza vencedores.
      (aux hand-list jogadores (list (car jogadores))))))

(define versus ;recebe duas hands, devolve 1 se hand1 for melhor, devolve 2 se hand2 for melhor, tie se for empate
  (lambda (hand1 hand2)
    (case (car hand1)
      ((0) (desempata-high-pairs-three-flush hand1 hand2))
      ((1) (desempata-high-pairs-three-flush hand1 hand2))
      ((2) (desempata-high-pairs-three-flush hand1 hand2))
      ((3) (desempata-high-pairs-three-flush hand1 hand2))
      ((4) (desempata-straight hand1 hand2))
      ((5) (desempata-high-pairs-three-flush hand1 hand2))
      ((6) (desempata-full-four hand1 hand2))
      ((7) (desempata-full-four hand1 hand2))
      ((8) (desempata-straight hand1 hand2))
      ((9) 'tie)))) ;nao existem criterios de desempate para royal straight flush

(define desempata-high-pairs-three-flush ;compara duas listas.
  (lambda (hand1 hand2)
    (letrec ((aux
              (lambda (lista1 lista2)
                (cond ((null? lista1) 'tie)
                      ((> (car lista1) (car lista2)) 1)
                      ((< (car lista1) (car lista2)) 2)
                      (else (aux (cdr lista1) (cdr lista2)))))))
      (aux (cdr hand1) (cdr hand2))))) ;0 primeiro elemento e igual

(define desempata-straight ;(straight + straight flush) analisa apenas qual o maior elemento da sequencia
  (lambda (hand1 hand2)
    (let ((elem1 (cdr hand1))
          (elem2 (cdr hand2)))
      (cond ((> elem1 elem2) 1)
            ((< elem1 elem2) 2)
            (else 'tie)))))

(define desempata-full-four ;compara dois pares, comecando pelo primeiro elemento.
  (lambda (hand1 hand2)
    (cond ((> (cadr hand1) (cadr hand2)) 1) ;compara o primeiro elemento do par de desempate
          ((< (cadr hand1) (cadr hand2)) 2)
          ((> (cddr hand1) (cddr hand2)) 1) ;compara o segundo elemento
          ((< (cddr hand1) (cddr hand2)) 2)
          (else 'tie))))

(define disp-vencedores ;devolve no ecra quem ganhou e a sua hand. altera o dinheiro dos vencedores.
  (lambda (vencedores winning-hand pot) ;lista com os numeros dos vencedores. winning-hand em numero
    (letrec ((aux
              (lambda (lista)
                (if (null? (cdr lista))
                    (begin
                      (display "e ")
                      (display (car lista)))
                    (begin                   
                      (display (car lista))
                      (if (null? (cddr lista))
                          (display " ")
                          (display ", "))
                      (aux (cdr lista)))))))
      (if (= (length vencedores) 1)
          (begin
            (vector-set! bank (sub1 (car vencedores)) (+ pot (vector-ref bank (sub1 (car vencedores))))) ;soma o pot ao dinheiro do vencedor.
            (display "O jogador ")
            (display (car vencedores))
            (display " ganhou ") (display pot) (display "€ com: ")
            (num->hand winning-hand))
          (begin
            (split-pot! vencedores pot) ;divide o pot pelos vencedores.
            (display "Ganham os jogadores ")
            (aux vencedores)
            (display " com um: ")
            (num->hand winning-hand)
            (display ". Terao de dividir ") (display pot) (display "€")))
      (newline)
      (clear-bankrupt))))



;; Funcoes auxiliares inuteis ;;

(define vector->list-exceto ;transforma vetor numa list, deixando apenas as posicoes de cada elemento de lista. Auxiliar de (desempata)
  (lambda (vetor lista)
    (letrec ((aux
              (lambda (i)
                (if (not (= i (vector-length vetor)))
                    (if (member (add1 i) lista)
                        (aux (add1 i))  ;se a posicao pertencer deixa-se estar
                        (begin ;else, poe-se a posicao a 0.
                          (vector-set! vetor i 0)
                          (aux (add1 i))))))))
      (aux 0) ;coloca as posicoes a remover a 0
      (apaga-zeros (vector->list vetor)))))

(define apaga-zeros ;nao funciona se for tudo 0. para listas.
  (lambda (lista)
    (letrec ((aux
              (lambda (l)
                (cond ((null? (cdr l)) lista)
                      ((and (integer? (cadr l)) (= (cadr l) 0)) (set-cdr! l (cddr l))
                                                                (aux l))
                      (else (aux (cdr l)))))))
      (if (equal? (car lista) 0)
          (begin
            (set-car! lista (cadr lista))
            (set-cdr! lista (cddr lista))
            (apaga-zeros lista))
          (aux lista)))))

(define apaga-uns ;nao funciona se for tudo 1. para listas.
  (lambda (lista)
    (letrec ((aux
              (lambda (l)
                (cond ((null? (cdr l)) lista)
                      ((equal? (cadr l) 1) (set-cdr! l (cddr l))
                                           (aux l))
                      (else (aux (cdr l)))))))
      (if (equal? (car lista) 1)
          (begin
            (set-car! lista (cadr lista))
            (set-cdr! lista (cddr lista))
            (apaga-uns lista))
          (aux lista)))))

(define temp-first ;e chamada sempre que se comeca uma rodada de apostas.
  (lambda (old-first) ;recebe o indice do jogador que era primeiro. (0-7 caso 8 jogadores)
    (letrec ((aux
              (lambda (i)
                (cond ((equal? (jogador (add1 i)) 0) (aux (remainder (add1 i) (vector-length jogadores)))) ;se  jogador tiver dado fold, passa-se para o proximo jogador (mod length-jog)
                      (else i))))) ;se nao, o primeiro jogador pode ser esse.
      (aux old-first))))






;;;;;;;;;;;;;;;
;; Ordenacao ;;
;;;;;;;;;;;;;;;

(define ordena-cartas ;DEVOLVE uma lista de cartas ordenada. Recebe uma mao (normalmente de 7 cartas). A ordem dos naipes NAO e considerada. (Selection Sort)
  (lambda (mao)
    (letrec ((aux ;vai vendo o mais pequeno da lista, apaga-o, adiciona-o a lista nova, repete.
              (lambda (lista nlista)               
                (if (null? lista) 
                    nlista
                    (begin
                      (let ((mais-peq (menor-carta lista))) ;mais-peq<-menor carta de lista       
                        (aux (elim-prim-ocorrencia lista mais-peq) (append nlista (list mais-peq)))))))))  ;chama ordena-lista com lista sem mais-peq, e adicionando essa carta a nlista
      (aux mao '()))))

(define menor-carta ;numa mao, ve qual e a menor CARTA. devolve-a
  (lambda (mao)
    (letrec ((aux
              (lambda (lista carta)
                (cond ((null? lista) carta)
                      ((< (caar lista) (car carta)) (aux (cdr lista) (car lista))) ;se o numero da carta for menor, elem<-carta
                      (else (aux (cdr lista) carta))))))
      (aux (cdr mao) (car mao)))))

(define elim-prim-ocorrencia
  (lambda (lista elemento)
    (letrec ((aux
              (lambda (lista nlista)
                (cond ((null? lista) nlista) ;devolve lista original
                      ((equal? (car lista) elemento) (append nlista (cdr lista))) ; ao encontrar o elemento junta a lista original até ao elemento e lista original depois do elemento
                      (else (aux (cdr lista) (append nlista (list (car lista))))))))) 
      (aux lista '()))))

(define set-maos! ;constroi a lista maos.
  (lambda ()
    (letrec ((aux
              (lambda (i)
                (if (not (= i (vector-length jogadores)))
                    (if (equal? (vector-ref jogadores i) 0) ;ve se o jogador desistiu. se sim, a mao dele e (0 0)
                        (begin
                          (append! maos (list '()))
                          (aux (add1 i)))
                        (begin
                          (append! maos (list (ordena-mao (jogador (add1 i))))) ;adiciona a mao do jogador i+1 a lista maos. (note-se que i comeca em 0 mas jogador comeca em 1).
                          (aux (add1 i)))))))
             (ordena-mao ;devolve as 7 cartas que compoem o jogo do jogador, ordena-as.
              (lambda (jogador)
                (ordena-cartas (append jogador (cdr mesa))))))
      (if (equal? (vector-ref jogadores 0) 0)
          (set! maos (list '()))
          (set! maos (list (ordena-mao (jogador 1))))) ;para que o append! nao trabalhe com listas vazias
      (aux 1))))

(define ordena-naipes ;recebe mao ordenada de 7 cartas. modifica o vetor naip-ord
  (lambda (mao)
    (letrec ((aux
              (lambda (lista)
                (if (not (null? lista))
                    (begin
                      (case (cdar lista)
                        ((Copas) (vector-set! naip-ord 0 (append (vector-ref naip-ord 0) (list (caar lista)))))
                        ((Espadas) (vector-set! naip-ord 1 (append (vector-ref naip-ord 1) (list (caar lista)))))
                        ((Ouros) (vector-set! naip-ord 2 (append (vector-ref naip-ord 2) (list (caar lista)))))
                        ((Paus) (vector-set! naip-ord 3 (append (vector-ref naip-ord 3) (list (caar lista))))))
                      (aux (cdr lista)))))))
      (aux mao))))

(define clear-naipes ;apaga o vetor
  (lambda ()
    (set! naip-ord (vector '() '() '() '()))))

;;;;;;;;
;; UI ;;
;;;;;;;;

(define mensagem-escolher-bet-check
  (lambda (pot)
    (display "------------------------------\n") (espera 150)
    (display "Banco: ") (display (vector-ref bank 0)) (display "€")
    (display " | Pot: ") (display pot) (display "€")
    (display " | ") (display (num-jogadores)) (display " pessoas em jogo") (espera 150)
    (disp-user) (espera 150)
    (disp-mesa) (espera 150)    
    (display "------------------------------\n") (espera 150)
    (display "O que deseja fazer?\n") (espera 150)
    (display "Bet | Check\n")))

(define mensagem-escolher-call-raise-fold
  (lambda (pot bet) ;tem de receber pot e bet pois da display aos dois.
    (display "------------------------------\n") (espera 150)
    (display "Banco: ") (display (vector-ref bank 0)) (display "€")
    (display " | Pot: ") (display pot) (display "€")
    (display " | ") (display (num-jogadores)) (display " pessoas em jogo") (espera 150)
    (disp-user) (espera 150)  
    (disp-mesa) (espera 150)
    (display "------------------------------\n") (espera 150)
    (display "O que deseja fazer?\n") (espera 150)
    (display "Call (-") (display (min (left-to-bet 0 bet) (vector-ref bank 0))) (display "€) | Raise (-") (display (min (left-to-bet 0 (ceiling (* 1.3 bet))) (vector-ref bank 0))) (display "€) | All-In (-") (display (vector-ref bank 0)) (display "€) | Fold\n")))
;min usado caso o dinheiro que haja no banco seja menor do que o dinheiro a apostar.

(define mensagem-recomecando-ronda
  (lambda ()
    (espera 2000) (display "Recomecando a ronda em 3...") (espera 1000)  (display "2...") (espera 1000)  (display "1...") (espera 1000)))

;;;;;;;;;;;;;;;;;;;;;;
;; Listas e vetores ;;
;;;;;;;;;;;;;;;;;;;;;;

(define baralha! ;reinicia o baralho
  (lambda ()
    (set! baralho (vector (cons 2 'Copas) (cons 2 'Espadas) (cons 2 'Ouros) (cons 2 'Paus)
                          (cons 3 'Copas) (cons 3 'Espadas) (cons 3 'Ouros) (cons 3 'Paus)
                          (cons 4 'Copas) (cons 4 'Espadas) (cons 4 'Ouros) (cons 4 'Paus)
                          (cons 5 'Copas) (cons 5 'Espadas) (cons 5 'Ouros) (cons 5 'Paus)
                          (cons 6 'Copas) (cons 6 'Espadas) (cons 6 'Ouros) (cons 6 'Paus)
                          (cons 7 'Copas) (cons 7 'Espadas) (cons 7 'Ouros) (cons 7 'Paus)
                          (cons 8 'Copas) (cons 8 'Espadas) (cons 8 'Ouros) (cons 8 'Paus)
                          (cons 9 'Copas) (cons 9 'Espadas) (cons 9 'Ouros) (cons 9 'Paus)
                          (cons 10 'Copas) (cons 10 'Espadas) (cons 10 'Ouros) (cons 10 'Paus)
                          (cons 11 'Copas) (cons 11 'Espadas) (cons 11 'Ouros) (cons 11 'Paus)
                          (cons 12 'Copas) (cons 12 'Espadas) (cons 12 'Ouros) (cons 12 'Paus)
                          (cons 13 'Copas) (cons 13 'Espadas) (cons 13 'Ouros) (cons 13 'Paus)
                          (cons 14 'Copas) (cons 14 'Espadas) (cons 14 'Ouros) (cons 14 'Paus))))) ;11-Valete 12-Dama 13-Rei 14-As

(define baralho (vector)) 
(define jogadores (vector))
(define mesa (list 'Mesa:))
(define maos (list)) ;7 cartas
(define naip-ord (vector '() '() '() '())) ;vetor de 4 posicoes. cada posicao contem uma lista com: Copas Espadas Ouros Paus
(define temp 0) ;Variavel temporaria auxiliar, acessivel por qualquer funcao.
(define temp2 0) ;apenas necessaria a segunda uma vez.

;;;;;;;;;;;;
;; TESTES ;;
;;;;;;;;;;;;

(define mao-boa0 (list (cons 3 'Paus) (cons 5 'Copas) (cons 6 'Paus) (cons 7 'Espadas) (cons 11 'Ouros) (cons 12 'Espadas) (cons 14 'Paus)))
(define mao-boa1 (list (cons 3 'Paus) (cons 5 'Copas) (cons 10 'Paus) (cons 11 'Espadas) (cons 11 'Ouros) (cons 12 'Espadas) (cons 14 'Paus)))
(define mao-boa2 (list (cons 3 'Paus) (cons 5 'Copas) (cons 10 'Paus) (cons 11 'Espadas) (cons 11 'Ouros) (cons 14 'Espadas) (cons 14 'Paus)))
(define mao-boa3 (list (cons 3 'Paus) (cons 5 'Copas) (cons 10 'Paus) (cons 11 'Espadas) (cons 11 'Ouros) (cons 11 'Espadas) (cons 14 'Paus)))
(define mao-boa4 (list (cons 3 'Paus) (cons 5 'Copas) (cons 10 'Paus) (cons 11 'Espadas) (cons 12 'Ouros) (cons 13 'Espadas) (cons 14 'Paus)))
(define mao-boa5 (list (cons 3 'Paus) (cons 5 'Paus) (cons 6 'Paus) (cons 7 'Paus) (cons 11 'Ouros) (cons 12 'Espadas) (cons 14 'Paus)))
(define mao-boa6 (list (cons 3 'Paus) (cons 3 'Copas) (cons 6 'Paus) (cons 7 'Espadas) (cons 11 'Ouros) (cons 11 'Espadas) (cons 11 'Paus)))
(define mao-boa7 (list (cons 3 'Paus) (cons 3 'Copas) (cons 3 'Ouros) (cons 3 'Espadas) (cons 11 'Ouros) (cons 11 'Espadas) (cons 11 'Paus)))
(define mao-boa8 (list (cons 3 'Paus) (cons 5 'Espadas) (cons 6 'Espadas) (cons 7 'Espadas) (cons 8 'Espadas) (cons 9 'Espadas) (cons 14 'Paus)))
(define mao-boa9 (list (cons 3 'Paus) (cons 5 'Copas) (cons 10 'Paus) (cons 11 'Paus) (cons 12 'Paus) (cons 13 'Paus) (cons 14 'Paus)))

(JOGA)
