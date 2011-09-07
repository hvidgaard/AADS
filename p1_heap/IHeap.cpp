#include <AADS.h>

class IHeap {
	public:
		virtual ~IHeap() {}
		virtual void initialize_heap() = 0;
		virtual void insert(Element* e) = 0;
		virtual Element* find_min() = 0;
		virtual Element* delete_min() = 0;
		virtual IHeap* meld(IHeap* h) = 0;
		virtual Element* decrease_key(int delta, Element* e) = 0;
		//virtual Element* delete(Element
};
