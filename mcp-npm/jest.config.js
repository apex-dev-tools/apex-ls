module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
  testMatch: [
    '**/test/**/*.test.ts'
  ],
  collectCoverageFrom: [
    'src/**/*.ts',
    'dist/**/*.js',
    '!src/**/*.d.ts',
    '!dist/**/*.d.ts',
    '!test/**/*'
  ],
  coverageDirectory: 'coverage',
  coverageReporters: ['text', 'lcov', 'html'],
  setupFilesAfterEnv: ['<rootDir>/test/helpers/setup.ts'],
  testTimeout: 30000, // 30 seconds for integration tests
  maxWorkers: 1, // Sequential execution like the main project
  modulePathIgnorePatterns: ['<rootDir>/dist/'],
  transform: {
    '^.+\\.ts$': 'ts-jest',
    '^.+\\.js$': 'babel-jest'
  }
};