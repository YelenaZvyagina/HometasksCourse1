using System;
using System.Threading.Tasks;
using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Markup.Xaml;
using Avalonia.Threading;

namespace Gui.Views
{
    public class MainWindow : Window
    {
        public static TextBox CodeInput = null!;
        private readonly TextBox _console;
        public static TextBox StatusBar = null!;
        public static string? Filename;

        public MainWindow()
        {
            InitializeComponent();
            Interpreter.printed.Subscribe(PrintToConsole);
            CodeInput = this.Find<TextBox>( "CodeInput");
            _console = this.Find<TextBox>( "Console");
            StatusBar = this.FindControl<TextBox>("StatusBar");
            Filename = "";
        }

        public static readonly MainWindow main = new ();

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }
        
        private void PrintToConsole(string s)
        {
            Dispatcher.UIThread.Post(() => _console.Text += s + '\n');
        }

        private void Run(object? sender, RoutedEventArgs routedEventArgs)
        {
            if (string.IsNullOrEmpty(CodeInput.Text))
            {
                StatusBar.Text = "Empty entry, nothing to compute";
                return;
            }
            StatusBar.Text = "Computing..";
            var task = new Task(() =>
            {
                try
                {
                    Interpreter.runPrint(Interpreter.parse(CodeInput.Text));
                    Dispatcher.UIThread.Post(() =>
                    {
                        StatusBar.Text = "Computed successfully";
                        _console.Text += "----------------------------------- \n";
                    });
                }
                catch (Exception ex)
                {
                    Dispatcher.UIThread.Post(() =>
                    {
                        StatusBar.Text = ex.Message;
                    });
                }
            });
            task.Start();
        }
    }
}
