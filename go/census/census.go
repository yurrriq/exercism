// Package census simulates a system used to collect census data.
package census

// Resident represents a resident in this city.
type Resident struct {
	Name    string
	Age     int
	Address map[string]string
}

// NewResident registers a new resident in this city.
func NewResident(name string, age int, address map[string]string) *Resident {
	return &Resident{
		Name:    name,
		Age:     age,
		Address: address,
	}
}

// HasRequiredInfo determines if a given resident has all of the required information.
func (r *Resident) HasRequiredInfo() (hasRequiredInfo bool) {
	if r.Name != "" && r.Address != nil {
		street, ok := r.Address["street"]
		hasRequiredInfo = ok && street != ""
	}

	return hasRequiredInfo
}

// Delete deletes a resident's information.
func (r *Resident) Delete() {
	r.Name = ""
	r.Age = 0
	r.Address = nil
}

// Count counts all residents that have provided the required information.
func Count(residents []*Resident) (count int) {
	for _, resident := range residents {
		if resident.HasRequiredInfo() {
			count++
		}
	}

	return count
}
