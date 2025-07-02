# inmydata Progress OpenEdge Sample Project

This sample project demonstrates how to integrate and use **inmydata OCX desktop components** within a **Progress OpenEdge GUI application**.

## Project Overview

The project includes sample `.w` files that illustrate how to:

- Embed inmydata OCX controls in a Progress GUI form
- Pass configuration data to the components
- Display copilot, insights, dashboards and visualisations from the inmydata platform
- Handle events and interactions using Progress ABL

This can be used as a reference when integrating inmydata controls into existing OpenEdge desktop applications.

## Files Included

- `main.w`: The main window and control initialisation - ** RUN THIS TO START
- `full.w` : Example of embedding the full inmydata UI
- `aichat.w`: Example of embedding the copilot view
- `dashboard.w`: Example of embedding a dashboard view
- `insights.w`: Example of embedding the insights view
- `visualisation.w` : Example of embedding the visualisation view
- `insightsViz.w`, `aiChatViz.w`: Supporting screens with various combinations of inmydata OCX controls

## Requirements

- Progress OpenEdge (classic GUI)
- inmydata OCX components installed and registered on the system [Download Here](https://download.inmydata.ai/inmydata.WindowsForms.Installer.msi)
- Access to an inmydata account

## Getting Started

1. Open the `.w` files in the AppBuilder or your OpenEdge development environment.
2. Ensure that the directory containing the `.w` files is in your PROPATH.
3. Run `main.w` to launch the application and test the integration.

## Developer Resources

- 🔧 **Developer Portal:** [https://developer.inmydata.com/support/home](https://developer.inmydata.com/support/home)  
  Access guides, tutorials, and developer support.

- 📘 **Controls API Reference:** [https://docs.inmydata.ai/desktop](https://docs.inmydata.ai/desktop)  
  Documentation for the available OCX desktop components, properties, and methods.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
