include "pixedit.h"
include "object.h"

# OBJ_CREATE -- Create a new object and initialize its fields

pointer procedure obj_create (parent, kind, fn)

pointer	parent		# i: parent object
int	kind		# i: kind of object
extern	fn		# i: function that will receive messages
#--
pointer	obj
int	locpr()

begin
	call malloc (obj, LEN_OBJSTRUCT, TY_STRUCT)

	OBJ_KIND(obj) = kind
	OBJ_DESCRIP(obj) = NULL
	OBJ_FUNC(obj) = locpr (fn)
	OBJ_PARENT(obj) = parent
	OBJ_CHILD(obj) = NULL

	if (parent == NULL) {
	    OBJ_NEXT(obj) = NULL
	} else {
	    OBJ_NEXT(obj) = OBJ_CHILD(parent)
	    OBJ_CHILD(parent) = obj
	}

	return (obj)
end

# OBJ_DESTROY -- Destroy an object and its children

procedure obj_destroy (obj)

pointer	obj		# i: Object descriptor
#--
pointer	child

begin
	# Only destroy the object if it has no children

	child = OBJ_CHILD(obj)
	if (child == NULL) {
	    call obj_remove (obj)
	    call mfree (obj, TY_STRUCT)

	# Otherwise send destroy messages to all the child objects
	# and the original object

	} else {
	    while (child != NULL) {
		call msg_send (M_DESTROY, child, NULL, NULL)
		child = OBJ_NEXT(child)
	    }
	    call msg_send (M_DESTROY, obj, NULL, NULL)
	}

end

# OBJ_DONE -- Free object search structure

procedure obj_done (osearch)

pointer	osearch		# i: object search structure
#--

begin
	call mfree (osearch, TY_STRUCT)
end

# OBJ_INSERT -- Insert an object before the specified object

procedure obj_insert (parent, obj, after)

pointer	parent		# i: object's new parent
pointer	obj		# i: object descriptor
pointer	after		# i: object that will follow obj
#--
pointer	child

begin
	if (obj == NULL)
	    return

	child = OBJ_CHILD(parent)
	if (child == after) {
	    OBJ_CHILD(parent) = obj
	    OBJ_NEXT(obj) = after

	} else {
	    while (OBJ_NEXT(child) != after)
		child = OBJ_NEXT(child)

	    OBJ_NEXT(child) = obj
	    OBJ_NEXT(obj) = after
	}

	OBJ_PARENT(obj) = parent
end

# OBJ_MOVE -- Move the location of an object in its parent's list

procedure obj_move (obj, after)

pointer	obj		# i: object descriptor
pointer	after		# i: object that will follow obj
#--
pointer	parent

begin
	parent = OBJ_PARENT(obj)

	call obj_remove (obj)
	call obj_insert (parent, obj, after)
end

# OBJ_NEXT -- Retrieve next object in depth first search order

pointer procedure obj_next (osearch)

pointer	osearch		# i: search structure
#--
int top
pointer obj

begin
	top = OS_TOP(osearch)
	if (top == 0)
	    return (NULL)

	repeat {
	    obj = OS_STACK(osearch,top)
	    if (obj == NULL)
		break

	    top = top + 1
	    OS_STACK(osearch,top) = OBJ_CHILD(obj)
	}

	top = top - 1
	if (top > 0) {
	    obj = OS_STACK(osearch,top)
	    OS_STACK(osearch,top) = OBJ_NEXT(obj)
	}

	OS_TOP(osearch) = top
	return (obj)
end

# OBJ_REMOVE --	Remove the object from its parent's list of children

procedure obj_remove (obj)

pointer	obj		# i: Object descriptor
#--
pointer	child, parent

begin
	if (obj == NULL)
	    return

	parent = OBJ_PARENT(obj)

	if (parent != NULL) {
	    child = OBJ_CHILD(parent)
	    if (child == obj) {
		OBJ_CHILD(parent) = OBJ_NEXT(obj)

	    } else {
		while (OBJ_NEXT(child) != obj)
		    child = OBJ_NEXT(child)
		OBJ_NEXT(child) = OBJ_NEXT(obj)
	    }
	}

	OBJ_PARENT(obj) = NULL
	OBJ_NEXT(obj) = NULL
end

# OBJ_SEARCH -- Initalize an object search structure

pointer procedure obj_search (obj)

pointer	obj		# i: object which acts as root of search
#--
pointer	osearch

begin
	call malloc (osearch, LEN_OSSTRUCT, TY_STRUCT)
	OS_TOP(osearch) = 1
	OS_STACK(osearch,1) = obj

	return (osearch)
end
	


