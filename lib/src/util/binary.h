#pragma once

#include "stream.h"

#include <iostream>
#include <stdexcept>
#include <string>

class read_error : public std::runtime_error {
 public:
  read_error(std::string message)
      : std::runtime_error(message) {}
};

class BinaryReader {
 public:
  BinaryReader(InputStream& input)
      : input_(input) {}

  // Encoded as a byte with value 0 (false) or 1 (true).
  bool readBool();

  // Little-endian encoded integers.
  int8_t readInt8();
  int16_t readInt16();
  int32_t readInt32();
  int64_t readInt64();

  uint8_t readUint8();
  uint16_t readUint16();
  uint32_t readUint32();
  uint64_t readUint64();

  // Variable-length encoding, in little-endian order, using the most
  // significant bit of each byte to signify whether another byte follows.
  int64_t readVarInt();
  uint64_t readVarUint();

  // IEEE-754 floating-point half-precision, single-precision, and
  // double-precision.
  float readFloat16();
  float readFloat();
  double readDouble();

  void readBytes(char buffer[], int64_t num_bytes);

  // Reads a varint representing the length, and then reads that many bytes
  // into the string.
  std::string readString();

  template <typename T> void read(T* value);
 private:
  double readFloating(uint64_t value, int bytes, int significand_bits);

  InputStream& input_;
};

class write_error : public std::runtime_error {
 public:
  write_error(std::string message)
      : std::runtime_error(message) {}
};

class BinaryWriter {
 public:
  BinaryWriter(OutputStream& output)
      : output_(output) {}

  // Encoded as a byte with value 0 (false) or 1 (true).
  void writeBool(bool value);

  // Little-endian encoded integers.
  void writeInt8(int8_t value);
  void writeInt16(int16_t value);
  void writeInt32(int32_t value);
  void writeInt64(int64_t value);

  void writeUint8(uint8_t value);
  void writeUint16(uint16_t value);
  void writeUint32(uint32_t value);
  void writeUint64(uint64_t value);

  // Variable-length encoding, in little-endian order, using the most
  // significant bit of each byte to signify whether another byte follows.
  void writeVarInt(int64_t value);
  void writeVarUint(uint64_t value);

  // IEEE-754 floating-point half-precision, single-precision, and
  // double-precision.
  void writeFloat16(float value);
  void writeFloat(float value);
  void writeDouble(double value);

  void writeBytes(const char buffer[], int64_t num_bytes);

  // Write a varint representing the string length, followed by the string
  // bytes.
  void writeString(const std::string& value);

  template <typename T> void write(const T& value);
 private:
  // Encodes a floating point number. The following restrictions apply:
  // 1 <= bytes <= 8, significand_bits <= 8 * bytes - 2,
  // significand_bits <= 52.
  // These are not checked.
  uint64_t encodeFloating(double value, int bytes, int significand_bits);

  OutputStream& output_;
};

// Provided specializations.
template <> void BinaryReader::read(bool* value);
template <> void BinaryReader::read(int8_t* value);
template <> void BinaryReader::read(int16_t* value);
template <> void BinaryReader::read(int32_t* value);
template <> void BinaryReader::read(int64_t* value);
template <> void BinaryReader::read(uint8_t* value);
template <> void BinaryReader::read(uint16_t* value);
template <> void BinaryReader::read(uint32_t* value);
template <> void BinaryReader::read(uint64_t* value);
template <> void BinaryReader::read(float* value);
template <> void BinaryReader::read(double* value);
template <> void BinaryReader::read(std::string* value);

template <> void BinaryWriter::write(const bool& value);
template <> void BinaryWriter::write(const int8_t& value);
template <> void BinaryWriter::write(const int16_t& value);
template <> void BinaryWriter::write(const int32_t& value);
template <> void BinaryWriter::write(const int64_t& value);
template <> void BinaryWriter::write(const uint8_t& value);
template <> void BinaryWriter::write(const uint16_t& value);
template <> void BinaryWriter::write(const uint32_t& value);
template <> void BinaryWriter::write(const uint64_t& value);
template <> void BinaryWriter::write(const float& value);
template <> void BinaryWriter::write(const double& value);
template <> void BinaryWriter::write(const std::string& value);
