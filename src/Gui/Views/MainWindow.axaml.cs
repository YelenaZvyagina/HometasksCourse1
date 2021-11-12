using System;
using System.Threading.Tasks;
using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Markup.Xaml;
using Avalonia.Threading;
using static Interpreter;

namespace Gui.Views
{
    public class MainWindow : Window
    {
        public static TextBox _codeInput;
        private readonly TextBlock _console;
        public static TextBlock _statusBar;
        public static string? filename;

        public MainWindow()
        {
            InitializeComponent();
            _codeInput = this.Find<TextBox>( "CodeInput");
            _console = this.Find<TextBlock>( "Console");
            _statusBar = this.FindControl<TextBlock>("StatusBar");
            filename = "";
        }

        public static readonly MainWindow main = new ();

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }

        private void Run(object? sender, RoutedEventArgs routedEventArgs)
        {
            if (string.IsNullOrEmpty(_codeInput.Text))
            {
                _statusBar.Text = "Empty entry, nothing to compute";
                return;
            }
            _statusBar.Text = "Computing";
            var task = new Task(() =>
            {
                try
                {
                    var (_, _, pd) = run(parse(_codeInput.Text));
                    Dispatcher.UIThread.Post(() =>
                    {
                        _console.Text = pd[outputBuffer];
                        _statusBar.Text = "Computed successfully";
                    });
                }
                catch (Exception ex)
                {
                    Dispatcher.UIThread.Post(() =>
                    {
                        _statusBar.Text = ex.Message;
                    });
                }
            });
            task.Start();
        }
    }
}
