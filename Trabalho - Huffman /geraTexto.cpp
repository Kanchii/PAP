#include <iostream>
#include <string.h>
#include <cstdio>
#include <cstdlib>
#include <time.h>

using namespace std;

int main(int argc, char const *argv[]){
	
	if(argc < 2){
		cout << "Digite o tamanho do arquivo" << endl;
		return -1;
	}

	int tamArq = atoi(argv[1]);

	if(argc > 2){
		if(strcmp(argv[2], "KB") == 0){
			tamArq *= 1024;
		} else if(strcmp(argv[2], "MB") == 0){
			tamArq *= 1024 * 1024;
		} else if(strcmp(argv[2], "GB") == 0){
			tamArq *= 1024 * 1024 * 1024;
		}
	}

	string text = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ \n";
	srand(time(NULL));

	for(int i = 0; i < tamArq; i++){
		cout << text[rand() % (text.size())];
	}

	return 0;
}