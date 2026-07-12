
Cree Value para poder aprovechar el pattern matching en Distributions , realmente no se que es mejor si ahorrarte Value pero tener que usar un sample distinto para cada distribucion(osea, que en lugar de llamar sample y te de cualquiera llamar por ejemplo a sampleNormal) o tener Value lo que te implica tener que poner Number 2 en lugar de 2 a secas.

No se si Haskell era la mejor alternativa , en un primer momento me pareció buena idea pero capaz otro lenguaje era mejor , me quedo bastante menos declarativo que lo que pensaba.

Es probable que haya sido redundante porque no tuve mucho tiempo para hacerlo y hace un tiempo que no programaba en haskell.
Tambien tuve problemas con las librerías porque se supone que System.Random.MWC.Distributions  tiene poisson , beta ,etc pero me decía que no estaban,supongo que era un problema de versiones de algo.

Tampoco se si lo hice del todo bien , trate de hacer algo similar a tener seed pero System.Random.MWC no la había usado nunca y no se si la parte de las semillas lo implemente bien.

Algunas distribuciones como la Dirichlet no se si estan bien porque las hice a partir de lo que encontre y lo que me iba explicando chat GPT de cómo implementarlas

Por un tema de tiempo los mensajes de error son malos y probablemente me hayan faltado casos.

Tambien en los test de los algoritmos puse menos pasos porque tardaba mucho,no se si es un tema de mi implementacion o de haskell.
