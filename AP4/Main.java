class Main {
  public static void main(String[] args) {
    Texto text = new Texto("Oi, meu nome eh Thiago! Thiago Augusto.");
    System.out.println(text.getTexto());
    System.out.println("Numero Palavras:" +text.getNumPalavras());
    
    
    text.changePalavra("Thiago", "thiago");
    System.out.println(text.getTexto());
    System.out.println("Numero Palavras:" +text.getNumPalavras());
    System.out.println("Frequencia 'thiago':" + text.freqPalavra("thiago"));
    System.out.println("Frequencia 'nome':" + text.freqPalavra("nome"));
    System.out.println("Frequencia 'helooo':" + text.freqPalavra("helooo"));
  }
}