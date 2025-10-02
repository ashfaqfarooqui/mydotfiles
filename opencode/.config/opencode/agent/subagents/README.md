# AI Agent System - Subagents

This directory contains the specialized AI agent definitions that form a collaborative multi-agent system for software development. Each agent has specific responsibilities and follows the BMAD (Break Down, Map, Act, Delegate) framework for coordinated task execution.

## Overview

The agent system implements a distributed architecture where specialized agents work together to handle complex software development workflows. Agents communicate through a structured messaging protocol and delegate tasks based on their capabilities and domain expertise.

## Agent Roster

### Core Orchestration
- **[Orchestrator Agent](orchestrator-agent.md)** - Central coordinator that manages workflow distribution, agent coordination, and high-level task planning
- **[Task Manager](task-manager.md)** - Tracks task lifecycle, manages priorities, and monitors execution state across all agents

### Development Agents
- **[Requirements Agent](requirements-agent.md)** - Analyzes and refines project requirements, creates specifications, and manages feature definitions
- **[Coding Agent](coding-agent.md)** - Implements code solutions, writes production code across multiple languages, and applies design patterns
- **[Code Analyzer Agent](code-analyzer-agent.md)** - Performs static analysis, quality assessment, security scanning, and technical debt monitoring
- **[Test Agent](test-agent.md)** - Creates and executes tests, generates test data, and validates code quality through automated testing

### Specialized Agents
- **[Git Workflow Agent](git-workflow-agent.md)** - Manages version control operations, branching strategies, PR workflows, and merge operations
- **[Documentation Agent](documentation-agent.md)** - Creates and maintains all technical documentation, API references, user guides, and architectural documentation
- **[Codebase Pattern Analyst](codebase-pattern-analyst.md)** - Analyzes code patterns, architectural decisions, and provides insights on codebase structure
- **[Coder Agent](coder-agent.md)** - Focused coding agent for rapid implementation and prototyping tasks

## Supported Languages

The agent system supports development in:
- **Rust** - Systems programming, performance-critical applications
- **Python** - Data processing, scripting, backend services
- **Scala** - Functional programming, distributed systems, data pipelines

## Architecture Principles

### BMAD Framework
Each agent implements the BMAD pattern:
1. **Break Down** - Decompose complex tasks into manageable subtasks
2. **Map** - Assign subtasks to appropriate agents and resources
3. **Act** - Execute assigned responsibilities with domain expertise
4. **Delegate** - Coordinate with other agents through structured messaging

### Communication Protocol
Agents communicate through a standardized messaging system:
- **Outbound Messages** - Events and notifications sent to other agents
- **Inbound Handlers** - Process requests and commands from other agents
- **Message Types** - Structured message formats for specific interactions

### Dependency Management
Agents declare dependencies on other agents:
```yaml
dependencies:
  - coding-agent.md
  - test-agent.md
  - task-manager.md
```

Dependencies determine coordination patterns and message routing.

## Agent Workflow

### Typical Development Flow
1. **Requirements Agent** analyzes and refines requirements
2. **Orchestrator Agent** creates high-level execution plan
3. **Task Manager** breaks down plan into tracked tasks
4. **Coding Agent** implements features and functionality
5. **Code Analyzer Agent** performs quality and security checks
6. **Test Agent** validates implementation with automated tests
7. **Documentation Agent** generates and updates documentation
8. **Git Workflow Agent** manages commits, branches, and PRs

### Quality Gates
Quality assurance is enforced at multiple stages:
- Code analysis before commit
- Test execution before merge
- Documentation validation before release
- Security scanning throughout development

## Agent Configuration

Each agent definition includes:
```yaml
---
id: agent-id
role: "Agent Role Name"
capabilities:
  - "Capability 1"
  - "Capability 2"
dependencies:
  - other-agent.md
languages: ["Rust", "Python", "Scala"]
---
```

## Integration Points

### CI/CD Integration
Agents integrate with continuous integration pipelines:
- Automated code analysis on commits
- Test execution on pull requests
- Documentation generation on merges
- Security scanning in deployment pipelines

### Development Tools
Agents leverage standard development tools:
- **Rust**: cargo, clippy, rustfmt, cargo-audit
- **Python**: pylint, pytest, black, mypy
- **Scala**: sbt, scalafmt, scalafix, scalastyle

### Version Control
All agents coordinate through git workflows:
- Feature branches for development
- Pull requests for code review
- Protected main branches with quality gates
- Automated merge on approval

## Adding New Agents

To add a new specialized agent:

1. Create a new markdown file: `new-agent.md`
2. Define agent metadata (id, role, capabilities, dependencies)
3. Implement BMAD responsibilities section
4. Specify inputs/outputs and messaging protocol
5. Document integration points and workflows
6. Update this README with agent description

## Best Practices

### Agent Design
- **Single Responsibility** - Each agent has a clear, focused purpose
- **Loose Coupling** - Agents communicate through messages, not direct calls
- **High Cohesion** - Related capabilities grouped within single agent
- **Explicit Dependencies** - Declare all inter-agent dependencies

### Communication
- **Async Messaging** - Prefer event-driven communication over synchronous calls
- **Structured Messages** - Use consistent message formats across agents
- **Error Handling** - Define failure modes and escalation paths
- **Idempotency** - Design message handlers to be safely retriable

### Testing
- Unit test individual agent capabilities
- Integration test agent communication patterns
- End-to-end test complete workflows
- Monitor agent performance and reliability metrics

## Troubleshooting

### Common Issues
- **Circular Dependencies** - Review agent dependencies for cycles
- **Message Deadlocks** - Check for synchronous blocking in async workflows
- **Quality Gate Failures** - Review agent outputs and fix issues before retry
- **Resource Contention** - Ensure agents can run concurrently without conflicts

### Debugging
- Review agent message logs for communication issues
- Check task manager state for stuck or failed tasks
- Validate agent inputs meet expected formats
- Monitor agent performance metrics for bottlenecks

## Future Enhancements

Planned improvements to the agent system:
- **Dynamic Agent Loading** - Load agents based on project requirements
- **Agent Learning** - Improve agent decisions based on historical outcomes
- **Custom Agent Creation** - Template system for domain-specific agents
- **Performance Optimization** - Parallel execution and resource management
- **Multi-Project Support** - Agent coordination across project boundaries

## Contributing

When modifying agents:
1. Maintain backward compatibility in message protocols
2. Update dependencies when adding inter-agent communication
3. Document new capabilities and workflows
4. Test integration with dependent agents
5. Update this README with significant changes

## License

Agent definitions and configurations are part of the broader project licensing.

## Support

For issues or questions about the agent system:
- Review individual agent documentation
- Check messaging protocol specifications
- Consult BMAD framework guidelines
- Reach out to system architects for design questions