#ifndef MYMAINWINDOW_H
#define MYMAINWINDOW_H

#include <QMainWindow>
#include "ui_mainwindow.h"

namespace Ui {
class MyMainWindow;
}
class MyMainWindow : public QMainWindow
{
    Q_OBJECT
public:
    explicit MyMainWindow(QWidget *parent = 0);
    ~MyMainWindow();
private:
    Ui_MainWindow *ui;
signals:

public slots:
};

#endif // MYMAINWINDOW_H
