# TPD Literature Review System

Automated weekly literature review system that fetches, classifies, and summarizes research papers from PubMed using LLMs. Generates PDF reports and sends email notifications.

## Features

- 🔍 **Automated PubMed Search**: Fetches papers based on configurable search terms
- 🤖 **LLM Classification**: Categorizes papers using local LLM (Ollama)
- 📝 **Smart Summaries**: Generates essay-style summaries focusing on aims and findings
- 📄 **PDF Reports**: Creates professional 3-page narrative reports
- 📧 **Email Delivery**: Automatically emails reports to recipients
- ⏰ **Weekly Scheduling**: Runs every Sunday (configurable)
- 🎯 **Topic Agnostic**: Easily adapt to any research domain via config

## Quick Start

### Prerequisites

1. **R** (version 4.0+)
2. **Ollama** - Install from [ollama.com](https://ollama.com)
3. **Git** (optional, for version control)

### Installation

1. **Clone the repository**
```bash
git clone https://github.com/yourusername/tpd_literature_review.git
cd tpd_literature_review
```

2. **Install R dependencies**
```r
source("install_dependencies.R")
```

3. **Install and start Ollama**
```bash
# Install from https://ollama.com
# Then pull the model
ollama pull llama3.2
```

4. **Configure the system**
- Edit `config.yaml` to customize:
  - PubMed search terms
  - Classification categories
  - Email settings
  - Schedule preferences

5. **Set up email credentials**
Create a `.Renviron` file in the project root:
```
EMAIL_PASSWORD="your_app_specific_password"
SMTP_USER="your_email@gmail.com"
```

### Usage

**Test the config loader:**
```r
source("modules/config_loader.R")
config <- load_config()
print_config_summary(config)
```

**Manual run (once other modules are implemented):**
```r
source("main.R")
result <- run_literature_review()
```

**Set up weekly schedule (once scheduler is implemented):**
```r
source("scheduler.R")
setup_schedule(load_config())
```

## Project Structure
```
tpd_literature_review/
├── config.yaml              # Main configuration file
├── install_dependencies.R   # Dependency installation script
├── modules/
│   ├── config_loader.R     # Configuration management
│   ├── pubmed_fetch.R      # [To be implemented]
│   ├── llm_classifier.R    # [To be implemented]
│   ├── llm_summarizer.R    # [To be implemented]
│   ├── report_compiler.R   # [To be implemented]
│   └── email_sender.R      # [To be implemented]
├── templates/
│   └── essay_summary_template.Rmd  # [To be implemented]
├── data/                    # Cached paper data
├── reports/                 # Generated PDF reports
├── logs/                    # Execution logs
└── tests/                   # Test files
```

## Configuration

All settings are in `config.yaml`. Key sections:

- **pubmed**: Search terms, date range, article limits
- **llm**: Model selection, timeouts, retries
- **classification**: Categories and prompts
- **summarization**: Summary generation prompts
- **report**: PDF formatting options
- **email**: SMTP settings and recipients
- **schedule**: When to run automatically

### Example: Adapting to a New Topic

To switch from TPD research to cancer immunotherapy:

1. Update `project_name`
2. Change `pubmed.search_terms`
3. Modify `classification.categories`
4. Update classification and summarization prompts

No code changes needed!

## Development Status

✅ **Chat 1: Foundation & Configuration** (Complete)
- Project structure
- Configuration system
- Dependency management

⏳ **Chat 2: PubMed Fetch Module** (Next)
⏳ **Chat 3: Classification Module**
⏳ **Chat 4: Summarization Module**
⏳ **Chat 5: Report Generation**
⏳ **Chat 6: Email & Main Workflow**
⏳ **Chat 7: Scheduler & Deployment**

## Requirements

- R >= 4.0
- Ollama (local LLM)
- LaTeX (TinyTeX, installed automatically)
- Internet connection (for PubMed API)

## License

MIT License

## Contributing

Contributions welcome! Please open an issue or submit a pull request.

## Acknowledgments

Built with:
- [rentrez](https://github.com/ropensci/rentrez) - PubMed API
- [ellmer](https://github.com/hadley/ellmer) - LLM interface
- [Ollama](https://ollama.com) - Local LLM runtime

---

**Current Status**: Foundation complete, ready for module implementation.