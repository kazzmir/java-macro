package com.rafkind.macro;

import java.io.*;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class Main{
    private static void process(String file){
        System.out.println("Processing " + file);
        try{
            FileInputStream stream = new FileInputStream(file);
            Syntax syntax = read(stream);
            stream.close();
        } catch (IOException fail){
            System.out.println("Could not process " + file);
            System.err.println(fail);
        }
    }

    private static Syntax read(InputStream stream) throws IOException {
        JavaLexer lexer = new JavaLexer(stream);
        Token token = lexer.next();
        while (token != Token.Eof){
            System.out.println("Token " + token);
            token = lexer.next();
        }
        return null;
    }

    public static void main(String... args){
        if (args.length < 1){
            System.out.println("Give a .java file as an argument");
        }

        process(args[0]);
    }
}

class Syntax{
}

class JavaLexer{
    Pattern identifierCharFirst = Pattern.compile("[a-zA-Z_]");
    Pattern digit = Pattern.compile("[0-9]");
    Pattern identifierChar = Pattern.compile(String.format("%1$s|%2$s", identifierCharFirst.toString(), digit.toString()));
    Pattern identifier = Pattern.compile(String.format("%1$s%2s*", identifierCharFirst.toString(), identifierChar.toString()));

    /* All the input as a string */
    String input;
    /* Index into the input. Used with Matcher regions */
    int position;

    public JavaLexer(InputStream stream) throws IOException {
        input = readInput(stream);
        position = 0;
    }

    /* Read the entire stream into a string */
    private String readInput(InputStream stream) throws IOException {
        StringBuilder builder = new StringBuilder();

        byte[] buffer = new byte[1024];
        int read = stream.read(buffer);
        while (read != -1){
            char[] out = new char[read];
            for (int i = 0; i < read; i++){
                out[i] = (char) buffer[i];
            }

            builder.append(out);

            read = stream.read(buffer);
        }

        return builder.toString();
    }

    Token next(){
        Matcher matcher = identifier.matcher(input);
        matcher.region(position, input.length());
        if (matcher.lookingAt()){
            // System.out.println(String.format("Matched from %d to %d", matcher.start(), matcher.end()));
            position = matcher.end();
            String out = input.substring(matcher.start(), matcher.end());
            return new Token.Identifier(out);
        }

        return Token.Eof;
    }
}

class Token{
    public static Token Eof = new Token();

    public Token(){
    }

    public static class Identifier extends Token{
        public Identifier(String value){
            this.value = value;
        }

        String value;

        public String toString(){
            return value;
        }

    }
}
