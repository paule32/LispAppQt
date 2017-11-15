#-------------------------------------------------
#
# Project created by QtCreator 2017-08-27T13:50:39
#
#-------------------------------------------------

QT       += widgets

TARGET = kallup
TEMPLATE = lib

DEFINES += KALLUP_LIBRARY
DEFINES += QT_DEPRECATED_WARNINGS

SOURCES += kallup.cpp \
    mymainwindow.cpp

HEADERS += kallup.h\
        kallup_global.h \
    mymainwindow.h

unix {
    target.path = /usr/lib
    INSTALLS += target
}

FORMS += \
    mainwindow.ui
