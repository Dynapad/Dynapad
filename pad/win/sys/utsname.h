//
// Place holder until we figure out xdr stuff on windows
//

/*
 * If you are compiling the kernel, the value used in initializing
 * the utsname structure in kernel.cf must be the same as SYS_NMLN.
 */

#define _SYS_NMLN	257	/* 4.0 size of utsname elements.*/
				/* Must be at least 257 to 	*/
				/* support Internet hostnames.  */


struct utsname {
	char	sysname[_SYS_NMLN];
	char	nodename[_SYS_NMLN];
	char	release[_SYS_NMLN];
	char	version[_SYS_NMLN];
	char	machine[_SYS_NMLN];
	char	m_type[_SYS_NMLN];
	char	base_rel[_SYS_NMLN];
	char	reserve5[_SYS_NMLN];		/* reserved for future use */
	char	reserve4[_SYS_NMLN];		/* reserved for future use */
	char	reserve3[_SYS_NMLN];		/* reserved for future use */
	char	reserve2[_SYS_NMLN];		/* reserved for future use */
	char	reserve1[_SYS_NMLN];		/* reserved for future use */
	char	reserve0[_SYS_NMLN];		/* reserved for future use */
};



extern void uname(utsname *);

void uname(utsname *) {};
