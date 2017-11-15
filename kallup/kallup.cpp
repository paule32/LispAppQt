#include <stdio.h>
#include <sys/types.h>

#include <QApplication>
#include <QMainWindow>
#define DLL_EXPORT __attribute__ ((visibility ("default")))

extern "C" {
void *kallup_application_id = nullptr;
QMainWindow *kallup_main_window = nullptr;

static int argc = 1;
QApplication*
kallup_init_app(void)
{
    printf("Hello World!\n");
    
    char *argv[2];
    
    argv[0] = new char[50];
    argv[1] = new char[50];

    strcpy(argv[0], "/home/jens/Projekte/ai/test/run");
    strcpy(argv[1], "ApplicationExecName");

    QApplication *app = new QApplication(argc, argv);
    kallup_application_id = app;
    return app;
}

QMainWindow*
kallup_init_window(void)
{
    kallup_main_window = new QMainWindow(0);
    kallup_main_window->show();
    return kallup_main_window;
}

int DLL_EXPORT kallup_exec_app(QApplication *app)
{
    printf("exec appli\n");
    return app->exec();
}
};
