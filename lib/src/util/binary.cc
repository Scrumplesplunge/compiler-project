#include "binary.h"

#include <float.h>
#include <math.h>
#include <memory>

using namespace std;

bool BinaryReader::readBool() {
  uint8_t value = readUint8();
  if (value >= 2) throw read_error("Value in stream is not a boolean.");
  return (value != 0);
}

int8_t BinaryReader::readInt8() {
  return static_cast<int8_t>(readUint8());
}

int16_t BinaryReader::readInt16() {
  return static_cast<int16_t>(readUint16());
}

int32_t BinaryReader::readInt32() {
  return static_cast<int32_t>(readUint32());
}

int64_t BinaryReader::readInt64() {
  return static_cast<int64_t>(readUint64());
}

uint8_t BinaryReader::readUint8() {
  char c;
  if (!input_.get(c)) return 0;
  return static_cast<uint8_t>(c);
}

uint16_t BinaryReader::readUint16() {
  uint16_t output = readUint8();
  return output | (static_cast<uint16_t>(readUint8()) << 8);
}

uint32_t BinaryReader::readUint32() {
  uint32_t output = readUint16();
  return output | (static_cast<uint32_t>(readUint16()) << 16);
}

uint64_t BinaryReader::readUint64() {
  uint64_t output = readUint32();
  return output | (static_cast<uint64_t>(readUint32()) << 32);
}

int64_t BinaryReader::readVarInt() {
  uint64_t encoded = readVarUint();
  return static_cast<int64_t>((encoded >> 1) ^ -(encoded & 1));
}

uint64_t BinaryReader::readVarUint() {
  uint64_t number = 0;
  uint64_t shift = 0;
  char temp;
  while (input_.get(temp)) {
    number |= static_cast<uint64_t>(temp & 0x7F) << shift;
    if ((temp & 0x80) == 0) return number;
    shift += 7;
  }
  throw read_error("Incomplete var-int in stream.");
}

float BinaryReader::readFloat16() { return readFloating(readUint16(), 2, 10); }
float BinaryReader::readFloat()   { return readFloating(readUint32(), 4, 23); }
double BinaryReader::readDouble() { return readFloating(readUint64(), 8, 52); }

void BinaryReader::readBytes(char buffer[], int64_t num_bytes) {
  if (input_.read(buffer, num_bytes) != num_bytes)
    throw read_error("Stream ended before block-read completed.");
}

string BinaryReader::readBytes(int64_t num_bytes) {
  unique_ptr<char[]> buffer(new char[num_bytes]);
  if (input_.read(buffer.get(), num_bytes) != num_bytes)
    throw read_error("Stream ended before string read completed.");
  return string(buffer.get(), num_bytes);
}

string BinaryReader::readString() {
  return readBytes(readVarUint());
}

template <> void BinaryReader::read(bool* value)     { *value = readBool(); }
template <> void BinaryReader::read(int8_t* value)   { *value = readInt8(); }
template <> void BinaryReader::read(int16_t* value)  { *value = readInt16(); }
template <> void BinaryReader::read(int32_t* value)  { *value = readInt32(); }
template <> void BinaryReader::read(int64_t* value)  { *value = readInt64(); }
template <> void BinaryReader::read(uint8_t* value)  { *value = readUint8(); }
template <> void BinaryReader::read(uint16_t* value) { *value = readUint16(); }
template <> void BinaryReader::read(uint32_t* value) { *value = readUint32(); }
template <> void BinaryReader::read(uint64_t* value) { *value = readUint64(); }
template <> void BinaryReader::read(float* value)    { *value = readFloat(); }
template <> void BinaryReader::read(double* value)   { *value = readDouble(); }

template <> void BinaryReader::read(string* value) {
  *value = move(readString());
}

double BinaryReader::readFloating(
    uint64_t value, int bytes, int significand_bits) {
  int exponent_bits = 8 * bytes - significand_bits - 1;
  // Read the components.
  uint64_t significand = value & ((1ULL << significand_bits) - 1);
  uint64_t exponent =
    (value >> significand_bits) & ((1ULL << exponent_bits) - 1);
  bool negative = value >> (8 * bytes - 1);
  if (exponent == (1ULL << exponent_bits) - 1) {
    // Edge case values.
    double out = significand ? NAN : INFINITY;
    return negative ? -out : out;
  }
  // Load the value from its components.
  int64_t raw_exponent = exponent + 1 - (1ULL << (exponent_bits - 1));
  double out = ldexp(significand, -significand_bits);
  if (exponent == 0) {
    out = ldexp(out, raw_exponent + 1);
  } else {
    out = ldexp(1 + out, raw_exponent);
  }
  return negative ? -out : out;
}

void BinaryWriter::writeBool(bool value) {
  writeUint8(value ? 1 : 0);
}

void BinaryWriter::writeInt8(int8_t value) {
  writeUint8(static_cast<uint8_t>(value));
}

void BinaryWriter::writeInt16(int16_t value) {
  writeUint16(static_cast<uint16_t>(value));
}

void BinaryWriter::writeInt32(int32_t value) {
  writeUint32(static_cast<uint32_t>(value));
}

void BinaryWriter::writeInt64(int64_t value) {
  writeUint64(static_cast<uint64_t>(value));
}

void BinaryWriter::writeUint8(uint8_t value) {
  char c = static_cast<char>(value);
  if (output_.write(&c, 1) != 1)
    throw write_error("Output stream closed unexpectedly.");
}

void BinaryWriter::writeUint16(uint16_t value) {
  writeUint8(value);
  writeUint8(value >> 8);
}

void BinaryWriter::writeUint32(uint32_t value) {
  writeUint16(value);
  writeUint16(value >> 16);
}

void BinaryWriter::writeUint64(uint64_t value) {
  writeUint32(value);
  writeUint32(value >> 32);
}

void BinaryWriter::writeVarInt(int64_t value) {
  uint64_t encoded = static_cast<uint64_t>(value);
  writeVarUint((encoded << 1) ^ (encoded >> 63));
}

void BinaryWriter::writeVarUint(uint64_t value) {
  do {
    char temp = value & 0x7F;
    value >>= 7;
    if (value) temp |= 0x80;
    if (!output_.put(temp))
      throw write_error("Output stream closed unexpectedly.");
  } while (value);
}

void BinaryWriter::writeFloat16(float value) {
  writeUint16(encodeFloating(value, 2, 10));
}

void BinaryWriter::writeFloat(float value) {
  writeUint32(encodeFloating(value, 4, 23));
}

void BinaryWriter::writeDouble(double value) {
  writeUint64(encodeFloating(value, 8, 52));
}

void BinaryWriter::writeBytes(const char buffer[], int64_t num_bytes) {
  if (output_.write(buffer, num_bytes) != num_bytes)
    throw write_error("Output stream closed unexpectedly.");
}

void BinaryWriter::writeString(const string& value) {
  writeVarUint(value.length());
  writeBytes(value.c_str(), value.length());
}

template <> void BinaryWriter::write(const bool& value) {
  writeBool(value);
}

template <> void BinaryWriter::write(const int8_t& value) {
  writeInt8(value);
}

template <> void BinaryWriter::write(const int16_t& value) {
  writeInt16(value);
}

template <> void BinaryWriter::write(const int32_t& value) {
  writeInt32(value);
}

template <> void BinaryWriter::write(const int64_t& value) {
  writeInt64(value);
}

template <> void BinaryWriter::write(const uint8_t& value) {
  writeUint8(value);
}

template <> void BinaryWriter::write(const uint16_t& value) { 
  writeUint16(value);
}

template <> void BinaryWriter::write(const uint32_t& value) {
  writeUint32(value);
}

template <> void BinaryWriter::write(const uint64_t& value) {
  writeUint64(value);
}

template <> void BinaryWriter::write(const float& value) {
  writeFloat(value);
}

template <> void BinaryWriter::write(const double& value) {
  writeDouble(value);
}

template <> void BinaryWriter::write(const string& value) {
  writeString(value);
}

uint64_t BinaryWriter::encodeFloating(
    double value, int bytes, int significand_bits) {
  int exponent_bits = 8 * bytes - significand_bits - 1;
  bool negative = signbit(value);
  if (negative) value = -value;
  uint64_t significand, exponent;
  if (isnan(value) || isinf(value)) {
    // Edge case values.
    exponent = -1;
    significand = isnan(value) ? 1 : 0;
  } else {
    int32_t raw_exponent;
    significand = static_cast<uint64_t>(
        ldexp(frexp(value, &raw_exponent), significand_bits + 1));
    // Convert the exponent into the biased form.
    int32_t limit = (1 << exponent_bits) - 1;
    raw_exponent += (1ULL << (exponent_bits - 1)) - 2;
    exponent = static_cast<uint64_t>(raw_exponent);
    if (raw_exponent >= limit) {
      // This value overflows the available space. Use infinity.
      exponent = -1;
      significand = 0;
    } else if (raw_exponent == 0) {
      // This value is denormalized.
      significand >>= 1;
    } else if (significand == 0 || raw_exponent < 0) {
      // This value can only be zero in this representation.
      exponent = significand = 0;
    }
  }
  return (significand & ((1ULL << significand_bits) - 1))
       | ((exponent & ((1ULL << exponent_bits) - 1)) << significand_bits)
       | (negative ? (1ULL << (8 * bytes - 1)) : 0ULL);
}
