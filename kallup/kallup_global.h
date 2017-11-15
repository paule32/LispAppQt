#ifndef KALLUP_GLOBAL_H
#define KALLUP_GLOBAL_H

#include <QtCore/qglobal.h>

#if defined(KALLUP_LIBRARY)
#  define KALLUPSHARED_EXPORT Q_DECL_EXPORT
#else
#  define KALLUPSHARED_EXPORT Q_DECL_IMPORT
#endif

#endif // KALLUP_GLOBAL_H
