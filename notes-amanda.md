# Anotações com Pedro sobre paper da Amanda (predição de MDD recorrente)

- Modelo de regressão logística regularizada (elastic net)
- Existe o risco das imputações se transformarem em um padrão
- Limitação teórica: desfecho complicado de prever
- Recorrência de ep. depressivo é muito complicado pois todos os sujeitos podem ser recorrentes (ser muito semelhantes)
- Predição com outro método (random forest) - mesmo resultado
- Possível solução: aumentar número de variáveis preditoras (inserir funcionamento, qualidade de vida, traumas, etc)

# Ideias
- Número de ep. (único ou recorrente) - vi
- Variável da idade de início (do primeiro ep. depressivo) - vi
- CTQ (trauma) - vi
- Sintomas separados (MINI) - vi
- Conversão para TB - vd
- Depressão grave - 24 na BDI, 0 - 11, 12 - 24 - vd
- Depressão disfuncional - disfuncional acima de 11 para bipolares - vd
- Testar por domínio o CTQ (agora)
- Dividir por cluster os transt. personalidade
- Depressão severa comparada com leve e tirar os moderados
- Funcional e disfuncional - independente do diagnóstico (colocar diagnóstico no modelo) - baixa cognição talvez

- MANDAR AMANHÃ DE NOITE
- Pessoal não está medicado - DIFERENCIAL

# Reunião dia 29/04/2021
- Transtorno personalidade
- Trauma geral
- Dois modelos - um para predizer recorrencia/persistencia - um modelo para predizer a depressão grave
- Excluir depressão grave do baseline e/ou com sintomas psicoticos atuais - nao rola
- Tirar os psicoticos

- Tirava uso abusivo, antidepressivo, psicoticos e depressão c suicide risk atual 

- Retirar psicoticos e suicide risk (plano ou tentativa de suicídio atual) (miniC04 e miniC05 / tpsicoticoatual)

- Modelo de recorrência (igual) e severidade (BDI > 24 & BDI <= 24)
