package cipher

// A Cipher can encode and decode a message.
type Cipher interface {
	Encode(string) string
	Decode(string) string
}
