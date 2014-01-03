
/* Run-time Target Specification.  */
#undef  TARGET_VERSION
#define TARGET_VERSION  fputs (" (AVR32 uClinux with ELF)", stderr)

/* We don't want a .jcr section on uClinux. As if this makes a difference... */
#define TARGET_USE_JCR_SECTION 0

/* Here we go. Drop the crtbegin/crtend stuff completely. */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC							\
  "%{!shared: %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s}"			\
  " %{!p:%{profile:gcrt1.o%s}"						\
  " %{!profile:crt1.o%s}}}} crti.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtn.o%s"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (AVR32_FLAG_NO_INIT_GOT)
