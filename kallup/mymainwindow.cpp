#include "mymainwindow.h"

MyMainWindow::MyMainWindow(QWidget *parent)
    : QMainWindow(parent)
{
    ui = new Ui::MainWindow;
    ui->setupUi(this);
}

MyMainWindow::~MyMainWindow()
{
    delete ui;
}
