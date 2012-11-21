package com.rafkind.macro;

public class Main{
    private static void process(String file){
        System.out.println("Processing " + file);
    }

    public static void main(String... args){
        if (args.length < 1){
            System.out.println("Give a .java file as an argument");
        }

        process(args[0]);
    }
}
