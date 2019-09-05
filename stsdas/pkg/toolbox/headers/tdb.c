#include <stdio.h>
#include <string.h>

#define  SZ_RECORD	132
#define  NO_CMD		0
#define  ADD_CMD	1
#define  DEL_CMD	2
#define  FIND_CMD	3
#define  QUIT_CMD	4

/* TDB -- A trivial database to demonstrate linked lists in C */

struct dbase_rec {
    struct dbase_rec	*next;
    char		*record;
};

typedef struct dbase_rec dbase;

void	drop_record (char *rec);
int 	cmp_record (char *key, char *rec);
dbase  *read_dbase (char *fname);
char   *find_dbase (dbase *db, char *key);
int	del_dbase (dbase **db, char *key);
int	add_dbase (dbase **db, char *rec);
void	write_dbase (dbase *db, char *fname);
void	drop_dbase (dbase **db);
int	parse_command (char *command, char *buffer);
int	main (int argc, char **argv);

char *
new_record (char *buffer) {
    char *rec = (char *) malloc (SZ_RECORD+1);

    strcpy (rec, buffer);
    return rec;
}

void
drop_record (char *rec) {
    free (rec);
}

int 
cmp_record (char *key, char *rec) {
    for ( ;*key && *rec && *rec == *key; rec ++, key ++)
	;

    if ((*key == ' ' || *key == '\0') && (*rec == ' ' || *rec == '\0'))
	return 0;

    return *key - *rec;
}

dbase *
read_dbase (char *fname) {
    char *nl = NULL;
    dbase *db = NULL;
    dbase *row = NULL;
    dbase *ptr = NULL;

    char buffer[SZ_RECORD+1];
    FILE *fd = fopen (fname, "r");

    if (fd == NULL)
	return db;

    while (fgets (buffer, SZ_RECORD, fd)) {
	nl = strchr (buffer, '\n');
	if (nl == NULL) {
	    printf ("Database record truncated:\n%s\n", buffer);
	} else {
	    *nl = '\0';
	}

	row = (dbase *) malloc (sizeof (dbase));
	row->next = NULL;
	row->record = new_record (buffer);

	if (db == NULL) {
	    db = row;

	} else {
	    for (ptr = db; ptr->next != NULL; ptr = ptr->next)
		;

	    ptr->next = row;
	}
    }

    fclose (fd);
    return db;
}

char *
find_dbase (dbase *db, char *key) {
    dbase *ptr = NULL;

    for (ptr = db; ptr != NULL; ptr = ptr->next) {
	if (cmp_record (key, ptr->record) == 0)
	    return ptr->record;
    }

    return NULL;
}

int
del_dbase (dbase **db, char *key) {
    dbase *ptr = NULL;
    dbase *prevptr = NULL;

    for (ptr = *db; ptr != NULL; ptr = ptr->next) {
	if (cmp_record (key, ptr->record) == 0)
	    break;

	prevptr = ptr;
    }

    if (ptr == NULL)
	return 0;

    if (prevptr != NULL) {
	prevptr->next = ptr->next;
    } else {
	*db = ptr->next;
    }

    drop_record (ptr->record);
    free (ptr);

    return -1;
}

int
add_dbase (dbase **db, char *rec) {
    int cmp;
    int status;
    dbase *ptr = NULL;
    dbase *row = NULL;
    dbase *prevptr = NULL;
    dbase *nextptr = NULL;

    row = (dbase *) malloc (sizeof (dbase));
    row->next = NULL;
    row->record = new_record (rec);

    cmp = 1;
    for (ptr = *db; ptr != NULL; ptr = ptr->next) {
	cmp = cmp_record (rec, ptr->record);
	if (cmp <= 0) 
	    break;

	prevptr = ptr;
    }

    if (cmp != 0) {
	nextptr = ptr;
	status = 1;

    } else {
	nextptr = ptr->next;
	status = 0;

	drop_record (ptr->record);
	free (ptr);
    }

    row->next = nextptr;

    if (prevptr != NULL) {
	prevptr->next = row;
    } else {
	*db = row;
    }

    return status;
}

void
write_dbase (dbase *db, char *fname) {
    dbase *ptr = NULL;
    FILE *fd = fopen (fname, "w");

    for (ptr = db; ptr != NULL; ptr = ptr->next)
	fprintf (fd, "%s\n", ptr->record);

    fclose (fd);
}

void
drop_dbase (dbase **db) {
    dbase *ptr = NULL;
    dbase *nextptr = NULL;

    for (ptr = *db; ptr != NULL; ptr = nextptr) {
	nextptr = ptr->next;
	drop_record (ptr->record);
	free (ptr);
    }

    *db = NULL;
}

int
parse_command (char *command, char *buffer) {
    int ic;
    int val;
    char *nl;
    char name[SZ_RECORD+1];

    nl = strchr (command, '\n');
    if (nl == NULL) {
	printf ("Command truncated:\n%s\n", command);
	return (NO_CMD);
    } else {
	*nl = '\0';
    }

    for (ic = 0; command[ic] != ' ' && command[ic] != '\0'; ic++)
	name[ic] = command[ic];

    name[ic] = '\0';
    if (command[ic] == ' ')
	ic = ic + 1;

    strcpy (buffer, &command[ic]);
    
    if (strcmp (name, "add") == 0) {
	val = ADD_CMD;
    } else if (strcmp (name, "del") == 0) {
	val = DEL_CMD;
    } else if (strcmp (name, "find") == 0) {
	val = FIND_CMD;
    } else if (strcmp (name, "quit") == 0) {
	val = QUIT_CMD;
    } else {
	val = NO_CMD;
    }

    return val;
}

int
main (int argc, char **argv) {
    char *fname = "phonelist.dat";
    char command[SZ_RECORD+1];
    char buffer[SZ_RECORD+1];
    int more, val, num;
    dbase *db = NULL;
    char *rec = NULL;

    if (argc > 1)
	fname = argv[1];

    db = read_dbase (fname);
    for (more = 1; more; ) {
	if (fgets (command, SZ_RECORD, stdin)) {
	    val = parse_command (command, buffer);
	} else {
	    val = QUIT_CMD;
	}

	switch (val) {
	case NO_CMD:
	    printf ("Unrecognized command:\n%s\n", command);
	    break;
	case ADD_CMD:
	    num = add_dbase (&db, buffer);
	    if (num == 0) {
		printf ("One row replaced\n");
	    } else {
		printf ("One row added\n");
	    }
	    break;
	case DEL_CMD:
	    num = del_dbase (&db, buffer);
	    if (num == 0) {
		printf ("Row not found\n");
	    } else {
		printf ("One row deleted\n");
	    }
	    break;
	case FIND_CMD:
	    rec = find_dbase (db, buffer);
	    if (rec == NULL) {
		printf ("Row not found\n");
	    } else {
		printf ("%s\n", rec);
	    }
	    break;
	case QUIT_CMD:
	    write_dbase (db, fname);
	    drop_dbase (&db);
	    more = 0;
	    break;
	}
    }

    return 0;
}
