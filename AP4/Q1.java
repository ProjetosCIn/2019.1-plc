import java.util.ArrayList;
import java.util.List;

public class Texto{

    private String conteudo;
    private int numPalavras;
    private List <String> palavras;

    public Texto(String texto){
      this.conteudo = texto;
      this.palavras = this.splitPalavras();
      this.numPalavras = this.countNumPalavras();
    }

    public String getTexto(){
      return this.conteudo;
    }

    public int getNumPalavras(){
      return this.numPalavras;
    }

    public int freqPalavra(String palavraFreq){
      int i = 0;
      for(String palavra: this.palavras){
        if(palavra.equals(palavraFreq))
          ++i;
      }
      return i;
    }

    public void changePalavra(String change, String toChange){
      for(int i = 0; i < this.palavras.size(); i++){
        if(this.palavras.get(i).equals(change)){
          this.palavras.set(i, toChange);
        }
      }
      this.conteudo = String.join("", this.palavras);
      this.numPalavras = this.countNumPalavras();
    } 

    private List<String> splitPalavras(){
      
      String temp = "";
      List<String> ans = new ArrayList<>();
      for(int i = 0; i < this.conteudo.length(); i++){
        char atual = this.conteudo.charAt(i);
        if(Character.isDigit(atual) || Character.isLetter(atual)){
          temp += atual;
        }else{
          // Adding the word and the ponctuation after
          if(temp != "")
            ans.add(temp);
          ans.add(Character.toString(atual));
          temp = "";
        }

        if(i == this.conteudo.length() - 1 && temp != ""){
          ans.add(temp);
        }
      }
      return ans;
    }

    private int countNumPalavras(){
      int count = 0;
      for(int i = 0; i < this.palavras.size(); i++){
        String current = this.palavras.get(i);
        if(current.length() > 1){
          count++;
        }
      }
      return count;
    }
}