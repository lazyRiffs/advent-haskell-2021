#!/bin/bash


function build()
{
  ghc ./Main.hs -odir bin -hidir bin -o bin/Main
}

function clean()
{
  rm ./bin/Main
  rm ./bin/Main.o
  rm ./bin/Main.hi

}

function run()
{
  ./bin/Main
}
