
// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-

#ifndef __javax_swing_JScrollPane$ScrollBar__
#define __javax_swing_JScrollPane$ScrollBar__

#pragma interface

#include <javax/swing/JScrollBar.h>
extern "Java"
{
  namespace javax
  {
    namespace swing
    {
        class JScrollPane;
        class JScrollPane$ScrollBar;
    }
  }
}

class javax::swing::JScrollPane$ScrollBar : public ::javax::swing::JScrollBar
{

public:
  JScrollPane$ScrollBar(::javax::swing::JScrollPane *, jint);
  virtual jint getBlockIncrement(jint);
  virtual jint getUnitIncrement(jint);
private:
  static const jlong serialVersionUID = -42032395320987283LL;
public: // actually package-private
  ::javax::swing::JScrollPane * __attribute__((aligned(__alignof__( ::javax::swing::JScrollBar)))) this$0;
public:
  static ::java::lang::Class class$;
};

#endif // __javax_swing_JScrollPane$ScrollBar__
