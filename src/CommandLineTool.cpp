

int main(int argc, const char **argv) {
  Sky_Frontend frontend = {};
  Sky_Frontend_Settings settings = {};
  sky_frontend_initalize(&frontend, &settings);

  if (argc < 2) {
    printf("Did not provide command line argument specificiying filename!");
    return -1;
  } else if (argc > 2) {
    printf(compiler, "Too many command line arguments!  Expected one filename!");
    return -1;
  }

  if (sky_frontend_add_file_cstring(&frontend, strlen(argv[1]))) {
    printf("could not open file");
  }

  if (sky_frontend_build(&frontend)) {
    sky_frontend_iterate_errors(&frontend, [](const char *message) {
      printf("%s", message);
    });
    return -1;
  }

  Sky_Backend_LLVM llvm_backend = {};
  sky_backend_llvm_initalize(&llvm_backend);
  sky_backend_llvm_run(&llvm_backend, &frontend)
  return 0;
}