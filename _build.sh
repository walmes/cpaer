#!/bin/bash

if [ "$1" = "-h" ]
then
    echo "./_build.sh       : rmarkdown::render_site(\"index.Rmd\")."
    echo "./_build.sh site  : rmarkdown::render_site()"
    echo "./_build.sh clean : rmarkdown::clean_site(preview = TRUE)"
    exit 1
fi

case "$1" in
    clean )
        echo "Arquivos que serão apagados:"
        Rscript -e "rmarkdown::clean_site(preview = TRUE)"
        echo "Deseja limpar os arquivos? [y/n]"
        read opcao;  echo $opcao
        if [ $opcao = "y" ];
        then
            echo "Limpando arquivos."
            Rscript -e "rmarkdown::clean_site()"
            echo "\n"
        fi
        ;;
    site )
        echo "Renderiza o site."
        Rscript -e "rmarkdown::render_site()"
        ;;
    * )
        echo "Renderizando o index.Rmd."
        Rscript -e "rmarkdown::render_site('index.Rmd')"
        ;;
esac
